#include "interm.h"
#include "gen.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "type.h"
#include "symtab.h"
#include "segment.h"
#include "tk.h"
#include "err.h"

#include <lt/io.h>
#include <lt/mem.h>
#include <lt/align.h>

#define ICODE_BLOCK_SIZE 512
#define ICODE_BLOCK_MASK (ICODE_BLOCK_SIZE-1)

#define SEGMENT_BLOCK_SIZE 512
#define SEGMENT_BLOCK_MASK (ICODE_BLOCK_SIZE-1)

static
usz emit(gen_ctx_t* cx, icode_t instr) {
	LT_ASSERT(cx->curr_func != -1);

	seg_ent_t* ent = &cx->seg[cx->curr_func];

	if (!(ent->size & ICODE_BLOCK_MASK))
		ent->data = realloc(ent->data, (ent->size + ICODE_BLOCK_SIZE) * sizeof(icode_t));

	((icode_t*)ent->data)[ent->size] = instr;
	return ent->size++;
}


usz new_code_seg(gen_ctx_t* cx, type_t* type) {
	if (!(cx->seg_count & SEGMENT_BLOCK_MASK))
		cx->seg = realloc(cx->seg, (cx->seg_count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	memset(&cx->seg[cx->seg_count], 0, sizeof(seg_ent_t));
	cx->seg[cx->seg_count].type = type;
	cx->seg[cx->seg_count].stype = SEG_ICODE;
	return cx->seg_count++;
}

usz new_data_seg(gen_ctx_t* cx, seg_ent_t new_ent) {
	if (!(cx->seg_count & SEGMENT_BLOCK_MASK))
		cx->seg = realloc(cx->seg, (cx->seg_count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	cx->seg[cx->seg_count] = new_ent;
	return cx->seg_count++;
}

#define REG_COUNT (cx->seg[cx->curr_func].regs)

static
u32 alloc_reg(gen_ctx_t* cx) {
	LT_ASSERT(cx->curr_func != -1);
	return ++REG_COUNT;
}

static
u32 immi_reg(gen_ctx_t* cx, usz size, u64 v) {
	u32 reg = alloc_reg(cx);
	emit(cx, ICODE(IR_INT, size, reg, .uint_val = v));
	return reg;
}

static
u32 ref_reg(gen_ctx_t* cx, usz size, u32 ref) {
	u32 reg = alloc_reg(cx);
	emit(cx, ICODE2(IR_LOAD, size, reg, ref));
	return reg;
}

static
u32 ival_reg(gen_ctx_t* cx, usz size, ival_t v) {
	switch (v.stype) {
	case IVAL_REG: return v.reg;
	case IVAL_IMM: return immi_reg(cx, size, v.uint_val);

	case IVAL_SEG: {
		u32 reg = alloc_reg(cx);
		emit(cx, ICODE2(IR_SEG, size, reg, v.uint_val));
		return reg;
	}

	case IVAL_SEG | IVAL_REF: {
		u32 ptr = alloc_reg(cx);
		emit(cx, ICODE2(IR_SEG, ISZ_64, ptr, v.uint_val));
		u32 reg = alloc_reg(cx);
		emit(cx, ICODE2(IR_LOAD, size, reg, ptr));
		return reg;
	}

	case IVAL_REG | IVAL_REF: return ref_reg(cx, size, v.reg);
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

static
u32 ival_ptr(gen_ctx_t* cx, ival_t v) {
	switch (v.stype) {
	case IVAL_SEG:
	case IVAL_SEG | IVAL_REF: {
		u32 reg = alloc_reg(cx);
		emit(cx, ICODE2(IR_SEG, ISZ_64, reg, v.uint_val));
		return reg;
	}

	case IVAL_REG:
	case IVAL_REG | IVAL_REF: return v.reg;
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

static
void* ival_data(gen_ctx_t* cx, ival_t* v) {
	switch (v->stype) {
	case IVAL_IMM: return &v->uint_val;
	case IVAL_SEG: return &cx->seg[v->uint_val].data;
	case IVAL_SEG | IVAL_REF: return cx->seg[v->uint_val].data;
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

u8* ival_write_comp(gen_ctx_t* cx, type_t* type, ival_t v, u8* out) {
	if (v.stype != IVAL_COM) {
		void* ptr = ival_data(cx, &v);
		usz size = type_bytes(type);
		memcpy(out, ptr, size);
		return out + size;
	}

	if (type->stype == TP_STRUCT) {
		type_t** types = type->children;

		for (usz i = 0; i < v.child_count; ++i) {
			type_t* memb_type = types[i];
			ival_t memb_v = v.children[i];
			out = ival_write_comp(cx, memb_type, memb_v, out);
		}
		return out;
	}
	else if (type->stype == TP_ARRAY_VIEW) {
		*(u64*)out = (u64)cx->seg[v.children[0].uint_val].data; // !!
		out += sizeof(u64);
		*(u64*)out = v.children[1].uint_val;
		return out + sizeof(u64);
	}
	else if (type->stype == TP_ARRAY) {
		usz child_count = v.child_count;
		for (usz i = 0; i < child_count; ++i)
			out = ival_write_comp(cx, type->base, v.children[i], out);
		return out;
	}

	LT_ASSERT_NOT_REACHED();
	return NULL;
}

static
ival_t gen_assign(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	u32 r1 = ival_ptr(cx, a1);

	if (size > ISZ_64) {
		u32 r2 = ival_ptr(cx, a2);
		emit(cx, ICODE2(IR_MCOPY, size, r1, r2));
		return a1;
	}
	else {
		u32 src = ival_reg(cx, size, a2);
		emit(cx, ICODE2(IR_STOR, size, src, r1));
		return REG(src);
	}
}

static
u32 reg_add_immi(gen_ctx_t* cx, u32 reg, u64 v) {
	u32 new_reg = alloc_reg(cx);
	u32 immi = immi_reg(cx, ISZ_64, v);
	emit(cx, ICODE3(IR_ADD, ISZ_64, new_reg, reg, immi));
	return new_reg;
}

static
u32 ival_gen_comp(gen_ctx_t* cx, type_t* type, ival_t v, u32 reg) {
	if (v.stype != IVAL_COM) {
		usz size = type_bytes(type);
		gen_assign(cx, size, REF(reg), v);
		reg = reg_add_immi(cx, reg, size);
	}

	if (type->stype == TP_STRUCT) {
		type_t** types = type->children;

		for (usz i = 0; i < v.child_count; ++i) {
			type_t* memb_type = types[i];
			ival_t memb_v = v.children[i];

			reg = ival_gen_comp(cx, memb_type, memb_v, reg);
		}
		return reg;
	}
	else if (type->stype == TP_ARRAY_VIEW) {
		gen_assign(cx, ISZ_64, REF(reg), v.children[0]);
		reg = reg_add_immi(cx, reg, sizeof(u64));
		gen_assign(cx, ISZ_64, REF(reg), v.children[1]);
		return reg_add_immi(cx, reg, sizeof(u64));
	}
	else if (type->stype == TP_ARRAY) {
		for (usz i = 0; i < v.child_count; ++i)
			reg = ival_gen_comp(cx, type->base, v.children[i], reg);
		return reg;
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

#define OP_GEN(name, instr, op) \
static \
ival_t gen_##name(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) { \
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) \
		return IMMI(a1.uint_val op a2.uint_val); \
 \
	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2); \
 \
	u32 dst = alloc_reg(cx); \
	emit(cx, ICODE3(instr, size, dst, r1, r2)); \
	return REG(dst); \
}

static
ival_t gen_add(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val + a2.uint_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = alloc_reg(cx);
	emit(cx, ICODE3(IR_ADD, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_sub(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val - a2.uint_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = alloc_reg(cx);
	emit(cx, ICODE3(IR_SUB, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_imul(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.int_val * a2.int_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = alloc_reg(cx);
	emit(cx, ICODE3(IR_IMUL, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_umul(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val * a2.uint_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = alloc_reg(cx);
	emit(cx, ICODE3(IR_UMUL, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_idiv(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.int_val / a2.int_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = alloc_reg(cx);
	emit(cx, ICODE3(IR_IDIV, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_udiv(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val / a2.uint_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = alloc_reg(cx);
	emit(cx, ICODE3(IR_UDIV, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_irem(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.int_val % a2.int_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = alloc_reg(cx);
	emit(cx, ICODE3(IR_IREM, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_urem(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val % a2.uint_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = alloc_reg(cx);
	emit(cx, ICODE3(IR_UREM, size, dst, r1, r2));
	return REG(dst);
}

OP_GEN(and, IR_AND, &)
OP_GEN(or, IR_OR, |)
OP_GEN(xor, IR_XOR, ^)

#define OP_GEN_SHIFT(name, instr, op, sign) \
static \
ival_t gen_##name(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) { \
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) \
		return IMMI(a1.sign##int_val op a2.sign##int_val); \
 \
	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, ISZ_8, a2); \
 \
	u32 dst = alloc_reg(cx); \
	emit(cx, ICODE3(instr, size, dst, r1, r2)); \
	return REG(dst); \
}

OP_GEN_SHIFT(ishl, IR_ISHL, <<, )
OP_GEN_SHIFT(ushl, IR_USHL, <<, u)
OP_GEN_SHIFT(ishr, IR_ISHR, >>, )
OP_GEN_SHIFT(ushr, IR_USHR, >>, u)

static
ival_t gen_static_compound(gen_ctx_t* cx, type_t* type, ival_t* v) {
	usz size = type_bytes(type);
	void* data = lt_arena_reserve(cx->arena, size);
	memset(data, 0, size);
	ival_write_comp(cx, type, *v, data);
	return IVAL(IVAL_SEG | IVAL_REF, .uint_val = new_data_seg(cx, SEG_ENT(SEG_DATA, CLSTR("@STATCOM"), size, data)));
}

static
ival_t gen_stack_compound(gen_ctx_t* cx, type_t* type, ival_t* v) {
	u32 reg = alloc_reg(cx);
	emit(cx, ICODE3(IR_SRESV, ISZ_64, reg, type_bytes(type), type_align(type)));
	ival_gen_comp(cx, type, *v, reg);
	return REF(reg);
}

static
void gen_sym_def(gen_ctx_t* cx, sym_t* sym, expr_t* expr) {
	if (sym->flags & SYMFL_CONST)
		return;

	if (sym->flags & SYMFL_GLOBAL) {

	}
	else {
		usz size = type_bytes(sym->type);
		usz align = type_align(sym->type);
		ival_t val = REF(alloc_reg(cx));
		emit(cx, ICODE3(IR_SRESV, ISZ_64, val.reg, size, align));

		if (expr) {
			if (size > ISZ_64)
				gen_assign(cx, size, val, icode_gen_expr(cx, expr));
			else if (expr)
				emit(cx, ICODE2(IR_STOR, size, ival_reg(cx, size, icode_gen_expr(cx, expr)), val.reg));
		}

		sym->val = val;
	}
}

static
b8 is_lval(ival_t v) {
	return v.stype & IVAL_REF;
}

static
void check_lval(gen_ctx_t* cx, tk_t* tk, ival_t a) {
	if (!is_lval(a))
		ferr("operator requires an lvalue", *tk);
}

#define FUNC_INSTR(x) (((icode_t*)cx->seg[cx->curr_func].data)[(x)])
#define CURR_IP() (cx->seg[cx->curr_func].size)

ival_t gen_const_expr(gen_ctx_t* cx, expr_t* expr) {
	// TODO: Evaluate constant expressions properly

	switch (expr->stype) {
	case EXPR_LAMBDA: {
		isz old_func = cx->curr_func;
		usz new_func = new_code_seg(cx, expr->type);
		cx->curr_func = new_func;

		cx->seg[cx->curr_func].name = CLSTR("@LAMDA");

		usz enter = emit(cx, ICODE0(IR_ENTER, ISZ_64));
		sym_t** args = expr->type->child_syms;
		isz arg_count = expr->type->child_count;
		usz stack_size = 0;
		for (isz i = arg_count - 1; i >= 0; stack_size += type_bytes(expr->type->children[i--])) {
			sym_t* sym = args[i];
			if (!sym /*|| !(sym->flags & SYMFL_ACCESSED)*/)
				continue;
			gen_sym_def(cx, sym, NULL);
			emit(cx, ICODE1(IR_GETARG, type_bytes(sym->type), sym->val.reg));
		}

		icode_gen_stmt(cx, expr->stmt);

		// TODO: Insert a ret only when not all code paths return
		emit(cx, ICODE0(IR_RET, 0));

		FUNC_INSTR(enter).dst = cx->seg[cx->curr_func].regs;

		cx->curr_func = old_func;
		return SEG(new_func);
	}

	case EXPR_STRUCT: {
		usz max_children = expr->type->child_count;
		ival_t* children = lt_arena_reserve(cx->arena, max_children * sizeof(ival_t));

		expr_t* it = expr->child_1;
		usz i = 0;
		for (; i < max_children && it; ++i) {
			children[i] = gen_const_expr(cx, it);
			it = it->next;
		}

		return IVAL(IVAL_COM, i, .children = children);
	}

	case EXPR_VIEW: {
		usz child_count = 2;
		ival_t* children = lt_arena_reserve(cx->arena, child_count * sizeof(ival_t));
		ival_t addr = icode_gen_expr(cx, expr->child_1);
		if (addr.stype == IVAL_COM)
			addr = gen_static_compound(cx, expr->child_1->type, &addr);
 		LT_ASSERT(addr.stype == (IVAL_SEG | IVAL_REF));

		children[0] = SEG(addr.uint_val);
		children[1] = IMMI(expr->child_1->type->child_count);
		return IVAL(IVAL_COM, child_count, .children = children);
	}

	case EXPR_ARRAY: {
		usz count = expr->type->child_count;
		ival_t* children = lt_arena_reserve(cx->arena, count * sizeof(ival_t));

		expr_t* it = expr->child_1;
		usz i = 0;
		for (; i < count && it; ++i) {
			children[i] = gen_const_expr(cx, it);
			it = it->next;
		}
		return IVAL(IVAL_COM, i, .children = children);
	}

	case EXPR_INTEGER:
		return IMMI(expr->uint_val);

	case EXPR_FLOAT:
		return IMMF(expr->float_val);

	case EXPR_STRING: {
		// TODO: Write some workaround to make this more space-efficient
		usz count = expr->str_val.len;
		ival_t* children = lt_arena_reserve(cx->arena, count * sizeof(ival_t));
		for (usz i = 0; i < count; ++i)
			children[i] = IMMI(expr->str_val.str[i]);

		return IVAL(IVAL_COM, count, .children = children);
	}

	case EXPR_CONVERT:
		return gen_const_expr(cx, expr->child_1);

	case EXPR_REFERENCE: {
		if (expr->child_1->stype != EXPR_SYM)
			goto expected_lval;
		ival_t ival = expr->child_1->sym->val;
		if (!is_lval(ival)) {
expected_lval:
			ferr("expected an lvalue", *expr->tk);
		}

		ival.stype &= ~IVAL_REF;
		return ival;
	}

	case EXPR_SYM:
		if (expr->sym->flags & SYMFL_CONST)
			return expr->sym->val;
	default:
		ferr("expected a compile-time constant", *expr->tk);
	}
}

ival_t icode_gen_expr(gen_ctx_t* cx, expr_t* expr) {
	switch (expr->stype) {
	// Literals
	case EXPR_LAMBDA:
	case EXPR_INTEGER:
	case EXPR_FLOAT:
	case EXPR_STRING:
		return gen_const_expr(cx, expr);

	case EXPR_SYM:
		return expr->sym->val;

	case EXPR_VIEW: {
		ival_t v = icode_gen_expr(cx, expr->child_1);
		if (v.stype == IVAL_COM)
			v = gen_static_compound(cx, expr->child_1->type, &v);

		u32 ptr = ival_ptr(cx, v);

		usz count = expr->child_1->type->child_count;
		u32 view_start = alloc_reg(cx);
		emit(cx, ICODE3(IR_SRESV, ISZ_64, view_start, 16, 8));

		gen_assign(cx, ISZ_64, REF(view_start), REG(ptr));
		u32 count_ptr = ival_reg(cx, ISZ_64, gen_add(cx, ISZ_64, REG(view_start), IMMI(8)));
		gen_assign(cx, ISZ_64, REF(count_ptr), IMMI(count));

		return REF(view_start);
	}

	case EXPR_ARRAY: {
		usz count = expr->type->child_count;
		usz elem_size = type_bytes(expr->type->base);
		usz size = count * elem_size;
		LT_ASSERT(cx->curr_func != -1);

		usz arr_start = alloc_reg(cx);
		emit(cx, ICODE3(IR_SRESV, ISZ_64, arr_start, size, type_align(expr->type)));

		expr_t* it = expr->child_1;
		for (usz i = 0; i < count && it; ++i) {
			ival_t a1 = icode_gen_expr(cx, it);

			ival_t member_start = gen_add(cx, ISZ_64, REG(arr_start), IMMI(i * elem_size));
			member_start.stype |= IVAL_REF;

			gen_assign(cx, elem_size, member_start, a1);

			it = it->next;
		}
		return REF(arr_start);
	}

	case EXPR_STRUCT: {
		usz size = type_bytes(expr->type);
		usz count = expr->type->child_count;
		LT_ASSERT(cx->curr_func != -1);

		u32 struct_start = alloc_reg(cx);
		emit(cx, ICODE3(IR_SRESV, ISZ_64, struct_start, size, type_align(expr->type)));

		usz stack_it = 0;
		expr_t* it = expr->child_1;

		for (usz i = 0; i < count && it; ++i) {
			usz elem_size = type_bytes(it->type);
			ival_t a1 = icode_gen_expr(cx, it);

			ival_t member_start = gen_add(cx, ISZ_64, REG(struct_start), IMMI(stack_it));
			member_start.stype |= IVAL_REF;

			gen_assign(cx, elem_size, member_start, a1);
			stack_it += elem_size;
			it = it->next;
		}
		return REF(struct_start);
	}

	// Binary
	case EXPR_ADD:
		return gen_add(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_SUBTRACT:
		return gen_sub(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_MULTIPLY:
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			return gen_imul(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_umul(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_DIVIDE:
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			return gen_idiv(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_udiv(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_MODULO:
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			return gen_irem(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_urem(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_BIT_AND:
		return gen_and(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_BIT_OR:
		return gen_or(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_BIT_XOR:
		return gen_xor(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_BIT_SHIFT_LEFT:
		if (is_int(expr->child_1->type))
			return gen_ishl(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_ushl(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_BIT_SHIFT_RIGHT:
		if (is_int(expr->child_1->type))
			return gen_ishr(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_ushr(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_CONVERT: {
		usz to = type_bytes(expr->type), from = type_bytes(expr->child_1->type);
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		if (to <= from || a1.stype == IVAL_IMM)
			return a1;

		u32 op = IR_INVAL;
		if (is_int(expr->type) && is_int(expr->child_1->type)) {
			switch (to) {
			case ISZ_16: op = IR_TOI16; break;
			case ISZ_32: op = IR_TOI32; break;
			case ISZ_64: op = IR_TOI64; break;
			}
		}
		else {
			switch (to) {
			case ISZ_16: op = IR_TOU16; break;
			case ISZ_32: op = IR_TOU32; break;
			case ISZ_64: op = IR_TOU64; break;
			}
		}
		u32 r1 = ival_reg(cx, from, a1), dst = alloc_reg(cx);
		emit(cx, ICODE2(op, from, dst, r1));
		return REG(dst);
	}

	// Unary
	case EXPR_NEGATE: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		if (a.stype == IVAL_IMM)
			return IMMI(-a.uint_val);
		usz size = type_bytes(expr->type);
		u32 r = ival_reg(cx, size, a), dst = alloc_reg(cx);
		emit(cx, ICODE2(IR_NEG, size, dst, r));
		return REG(dst);
	}

	case EXPR_PFX_INCREMENT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		usz size = type_bytes(expr->type);

		u32 ptr = ival_ptr(cx, a);
		u32 dst = alloc_reg(cx), val = ref_reg(cx, size, ptr);

		emit(cx, ICODE2(IR_INC, size, dst, val));
		emit(cx, ICODE2(IR_STOR, size, dst, ptr));
		return REG(dst);
	}

	case EXPR_PFX_DECREMENT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		usz size = type_bytes(expr->type);

		u32 ptr = ival_ptr(cx, a);
		u32 dst = alloc_reg(cx), val = ref_reg(cx, size, ptr);

		emit(cx, ICODE2(IR_DEC, size, dst, val));
		emit(cx, ICODE2(IR_STOR, size, dst, ptr));
		return REG(dst);
	}

	case EXPR_SFX_INCREMENT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		usz size = type_bytes(expr->type);

		u32 ptr = ival_ptr(cx, a);
		u32 tmp = alloc_reg(cx), val = ref_reg(cx, size, ptr);

		emit(cx, ICODE2(IR_INC, size, tmp, val));
		emit(cx, ICODE2(IR_STOR, size, tmp, ptr));
		return REG(val);
	}

	case EXPR_SFX_DECREMENT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		usz size = type_bytes(expr->type);

		u32 ptr = ival_ptr(cx, a);
		u32 tmp = alloc_reg(cx), val = ref_reg(cx, size, ptr);

		emit(cx, ICODE2(IR_DEC, size, tmp, val));
		emit(cx, ICODE2(IR_STOR, size, tmp, ptr));
		return REG(val);
	}

	case EXPR_BIT_NOT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		if (a.stype == IVAL_IMM)
			return IMMI(!a.uint_val);
		usz size = type_bytes(expr->type);
		u32 r = ival_reg(cx, size, a), dst = alloc_reg(cx);
		emit(cx, ICODE2(IR_NOT, size, dst, r));
		return REG(dst);
	}

	case EXPR_DEREFERENCE: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		return REF(ival_reg(cx, ISZ_64, a));
	}

	case EXPR_REFERENCE: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		return REG(ival_ptr(cx, a));
	}

	case EXPR_CALL: {
		ival_t ret_val = IVAL(0);
		usz ret_size = type_bytes(expr->type);
		usz ret_align = type_align(expr->type);

		if (expr->type->stype == TP_VOID)
			;
		else if (ret_size > 8) {
			u32 reg = alloc_reg(cx);
			emit(cx, ICODE3(IR_SRESV, ISZ_64, reg, ret_size, ret_align));
			ret_val = REF(reg);
		}
		else
			ret_val = REG(alloc_reg(cx));

		usz arg_count = expr->child_1->type->child_count;
		u32* arg_regs = lt_arena_reserve(cx->arena, arg_count * sizeof(u32));
		usz* arg_sizes = lt_arena_reserve(cx->arena, arg_count * sizeof(usz));

		expr_t* it = expr->child_2;
		for (usz i = 0; it; ++i, it = it->next) {
			usz size = type_bytes(it->type);
			arg_sizes[i] = size;
			ival_t ival = icode_gen_expr(cx, it);
			if (size > ISZ_64) {
				if (ival.stype == IVAL_COM)
					ival = gen_stack_compound(cx, it->type, &ival);
				arg_regs[i] = ival_ptr(cx, ival);
			}
			else
				arg_regs[i] = ival_reg(cx, size, ival);
		}

		u32 func_addr = ival_reg(cx, ISZ_64, icode_gen_expr(cx, expr->child_1));

		for (usz i = 0; i < arg_count; ++i)
			emit(cx, ICODE1(IR_SETARG, arg_sizes[i], arg_regs[i]));

		emit(cx, ICODE3(IR_CALL, ret_size, func_addr, ret_val.reg, REG_COUNT));
		return ret_val;
	}

	case EXPR_SUBSCRIPT: {
		LT_ASSERT(expr->child_1->type->stype == TP_PTR);

		ival_t ptr_val = icode_gen_expr(cx, expr->child_1), indx_val = icode_gen_expr(cx, expr->child_2);

		if (ptr_val.stype == IVAL_COM) {
			if (indx_val.stype != IVAL_IMM)
				ferr("Index to compile-time subscript must be constant", *expr->tk);
			u64 indx = indx_val.uint_val;
			if (indx >= ptr_val.child_count)
				ferr("Index out of range", *expr->child_2->tk);
			return ptr_val.children[indx];
		}

		usz size = type_bytes(expr->type);
		ival_t offs_val = gen_imul(cx, ISZ_64, indx_val, IMMI(size));
		ival_t res = gen_add(cx, ISZ_64, ptr_val, offs_val);
		u32 reg = ival_reg(cx, ISZ_64, res);
		return REF(reg);
	}

	case EXPR_MEMBER: {
		ival_t v = icode_gen_expr(cx, expr->child_1);
		if (v.stype == IVAL_COM) {
			LT_ASSERT(v.child_count > expr->member_index);
			return v.children[expr->member_index];
		}

		u32 ptr = ival_ptr(cx, v);
		LT_ASSERT(expr->child_1->type->child_count > expr->member_index);

		type_t** members = expr->child_1->type->children;
		usz member_offs = 0;
		for (usz i = 0; i < expr->member_index; ++i)
			member_offs += type_bytes(members[i]);

		ival_t ref = gen_add(cx, ISZ_64, REG(ptr), IMMI(member_offs));
		ref.stype |= IVAL_REF;
		return ref;
	}

	case EXPR_COUNT: {
		switch (expr->child_1->type->stype) {
		case TP_ARRAY:
			return IMMI(expr->child_1->type->child_count);
		case TP_ARRAY_VIEW: {
			ival_t val = icode_gen_expr(cx, expr->child_1);
			if (val.stype == IVAL_COM)
				return val.children[1];

			u32 reg = ival_ptr(cx, val);
			ival_t ref = gen_add(cx, ISZ_64, REG(reg), IMMI(8));
			ref.stype |= IVAL_REF;
			return ref;
		}
		default:
			LT_ASSERT_NOT_REACHED();
			return REG(0);
		}
	}

	case EXPR_DATA: {
		switch (expr->child_1->type->stype) {
		case TP_ARRAY: {
			ival_t val = icode_gen_expr(cx, expr->child_1);
			if (val.stype == IVAL_COM)
				return SEG(gen_static_compound(cx, expr->child_1->type, &val).uint_val);
			check_lval(cx, expr->tk, val);
			return REG(ival_ptr(cx, val));
		}
		case TP_ARRAY_VIEW: {
			ival_t val = icode_gen_expr(cx, expr->child_1);
			if (val.stype == IVAL_COM)
				return val.children[0];
			check_lval(cx, expr->tk, val);
			return val;
		}
		default:
			LT_ASSERT_NOT_REACHED();
			return REG(0);
		}
	}

	// Assigned binary
	case EXPR_ASSIGN: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		check_lval(cx, expr->tk, a1);
		return gen_assign(cx, type_bytes(expr->child_1->type), a1, a2);
	}

#define OP_ASSIGN(op) { \
	usz size = type_bytes(expr->child_1->type); \
	ival_t v, dst = icode_gen_expr(cx, expr->child_1); \
	check_lval(cx, expr->tk, dst); \
	v = gen_##op(cx, size, dst, icode_gen_expr(cx, expr->child_2)); \
	gen_assign(cx, size, dst, v); \
	return v; \
}

	case EXPR_ADD_ASSIGN:
		OP_ASSIGN(add)
	case EXPR_SUBTRACT_ASSIGN:
		OP_ASSIGN(sub)
	case EXPR_MULTIPLY_ASSIGN:
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			OP_ASSIGN(imul)
		else
			OP_ASSIGN(umul)
	case EXPR_DIVIDE_ASSIGN:
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			OP_ASSIGN(idiv)
		else
			OP_ASSIGN(udiv)
	case EXPR_MODULO_ASSIGN:
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			OP_ASSIGN(irem)
		else
			OP_ASSIGN(urem)
	case EXPR_BIT_SHIFT_LEFT_ASSIGN:
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			OP_ASSIGN(ishl)
		else
			OP_ASSIGN(ushl)
	case EXPR_BIT_SHIFT_RIGHT_ASSIGN:
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			OP_ASSIGN(ishr)
		else
			OP_ASSIGN(ushr)
	case EXPR_BIT_AND_ASSIGN:
		OP_ASSIGN(and)
	case EXPR_BIT_XOR_ASSIGN:
		OP_ASSIGN(xor)
	case EXPR_BIT_OR_ASSIGN:
		OP_ASSIGN(or)

	// Logical
	case EXPR_LOGIC_AND: {
		usz size = type_bytes(expr->type);
		u32 dst = immi_reg(cx, size, 0);

		u32 r1 = ival_reg(cx, size, icode_gen_expr(cx, expr->child_1));
		u32 trg = alloc_reg(cx);
		usz jmp1 = emit(cx, ICODE(IR_IPO, ISZ_64, trg, .int_val = 0));
		emit(cx, ICODE2(IR_CJMPZ, size, trg, r1));

		u32 r2 = ival_reg(cx, size, icode_gen_expr(cx, expr->child_2));
		emit(cx, ICODE2(IR_CSETNZ, size, dst, r2));

		FUNC_INSTR(jmp1).int_val = CURR_IP() - jmp1;
		return REG(dst);
	}

	case EXPR_LOGIC_OR: {
		usz size = type_bytes(expr->type);
		u32 dst = immi_reg(cx, size, 1);

		u32 r1 = ival_reg(cx, size, icode_gen_expr(cx, expr->child_1));
		u32 trg = alloc_reg(cx);
		usz jmp1 = emit(cx, ICODE(IR_IPO, ISZ_64, trg, .int_val = 0));
		emit(cx, ICODE2(IR_CJMPNZ, size, trg, r1));

		u32 r2 = ival_reg(cx, size, icode_gen_expr(cx, expr->child_2));
		emit(cx, ICODE2(IR_CSETNZ, size, dst, r2));

		FUNC_INSTR(jmp1).int_val = CURR_IP() - jmp1;
		return REG(dst);
	}

	case EXPR_LOGIC_NOT: {
		usz size = type_bytes(expr->type);
		u32 r = ival_reg(cx, size, icode_gen_expr(cx, expr->child_1));
		u32 dst = alloc_reg(cx);
		emit(cx, ICODE2(IR_CSETZ, size, dst, r));
		return REG(dst);
	}

	case EXPR_LESSER: {
		usz size = type_bytes(expr->type);
		u32 dst = alloc_reg(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) {
			if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
				return IMMI(a1.int_val < a2.int_val);
			else
				return IMMI(a1.uint_val < a2.uint_val);
		}

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			emit(cx, ICODE3(IR_CSETL, size, dst, r1, r2));
		else
			emit(cx, ICODE3(IR_CSETB, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_GREATER: {
		usz size = type_bytes(expr->type);
		u32 dst = alloc_reg(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) {
			if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
				return IMMI(a1.int_val > a2.int_val);
			else
				return IMMI(a1.uint_val > a2.uint_val);
		}

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			emit(cx, ICODE3(IR_CSETG, size, dst, r1, r2));
		else
			emit(cx, ICODE3(IR_CSETA, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_LESSER_OR_EQUAL: {
		usz size = type_bytes(expr->type);
		u32 dst = alloc_reg(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) {
			if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
				return IMMI(a1.int_val <= a2.int_val);
			else
				return IMMI(a1.uint_val <= a2.uint_val);
		}

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			emit(cx, ICODE3(IR_CSETLE, size, dst, r1, r2));
		else
			emit(cx, ICODE3(IR_CSETBE, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_GREATER_OR_EQUAL: {
		usz size = type_bytes(expr->type);
		u32 dst = alloc_reg(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) {
			if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
				return IMMI(a1.int_val >= a2.int_val);
			else
				return IMMI(a1.uint_val >= a2.uint_val);
		}

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		if (is_int(expr->child_1->type) && is_int(expr->child_2->type))
			emit(cx, ICODE3(IR_CSETGE, size, dst, r1, r2));
		else
			emit(cx, ICODE3(IR_CSETAE, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_EQUAL: {
		usz size = type_bytes(expr->type);
		u32 dst = alloc_reg(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IMMI(a1.uint_val == a2.uint_val);

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		emit(cx, ICODE3(IR_CSETE, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_NOT_EQUAL: {
		usz size = type_bytes(expr->type);
		u32 dst = alloc_reg(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IMMI(a1.uint_val != a2.uint_val);

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		emit(cx, ICODE3(IR_CSETNE, size, dst, r1, r2));
		return REG(dst);
	}

	// Misc.
	case EXPR_SYSCALL: {
		expr_t* it = expr->child_1;
		usz argc = 0;
		while (it) {
			emit(cx, ICODE1(IR_SETARG, ISZ_64, ival_reg(cx, ISZ_64, icode_gen_expr(cx, it))));
			it = it->next;
			++argc;
		}
		u32 dst = alloc_reg(cx);
		emit(cx, ICODE2(IR_SYSCALL, ISZ_64, dst, argc));
		return REG(dst);
	}

	default:
		break;
	}

	lt_ferrf("Unhandled expression type '%S'\n", expr_type_str(expr->stype));
}

void icode_gen_stmt(gen_ctx_t* cx, stmt_t* stmt) {
	switch (stmt->stype) {
	case STMT_LET: {
		stmt_t* it = stmt;
		while (it) {
			gen_sym_def(cx, it->sym, it->expr);
			it = it->child;
		}
	}	break;

	case STMT_EXPR:
		icode_gen_expr(cx, stmt->expr);
		break;

	case STMT_COMPOUND: {
		stmt_t* it = stmt->child;
		while (it) {
			icode_gen_stmt(cx, it);
			it = it->next;
		}
	}	break;

	case STMT_RETURN: {
		u32 reg = 0;
		usz size = 0;
		if (stmt->expr) {
			size = type_bytes(stmt->expr->type);
			if (size <= ISZ_64)
				reg = ival_reg(cx, size, icode_gen_expr(cx, stmt->expr));
			else {
				ival_t ival = icode_gen_expr(cx, stmt->expr);
				LT_ASSERT(ival.stype & IVAL_REF);
				reg = ival_ptr(cx, ival);
			}
		}
		emit(cx, ICODE1(IR_RET, size, reg));
		break;
	}


 	case STMT_DEF:
 		break;

	case STMT_WHILE: {
		usz start = CURR_IP();

		ival_t cond_v = icode_gen_expr(cx, stmt->expr);
		u32 cond_reg = ival_reg(cx, ISZ_8, cond_v);
		u32 trg = alloc_reg(cx);
		usz jmp1 = emit(cx, ICODE(IR_IPO, ISZ_64, trg, .int_val = 0));

		emit(cx, ICODE2(IR_CJMPZ, ISZ_8, trg, cond_reg));

		icode_gen_stmt(cx, stmt->child);

		trg = alloc_reg(cx);
		usz jmp2 = emit(cx, ICODE(IR_IPO, ISZ_64, trg, .int_val = 0));
		emit(cx, ICODE1(IR_JMP, ISZ_64, trg));

		FUNC_INSTR(jmp2).int_val = start - jmp2;

		FUNC_INSTR(jmp1).int_val = CURR_IP() - jmp1;
	}	break;

	case STMT_IF: {
		ival_t cond_v = icode_gen_expr(cx, stmt->expr);
		u32 cond_reg = ival_reg(cx, ISZ_8, cond_v);
		u32 trg = alloc_reg(cx);
		usz jmp1 = emit(cx, ICODE(IR_IPO, ISZ_64, trg, .int_val = 0));

		emit(cx, ICODE2(IR_CJMPZ, ISZ_8, trg, cond_reg));
		icode_gen_stmt(cx, stmt->child);

		if (stmt->child_2) {
			trg = alloc_reg(cx);
			usz jmp2 = emit(cx, ICODE(IR_IPO, ISZ_64, trg, .int_val = 0));
			emit(cx, ICODE1(IR_JMP, ISZ_64, trg));

			FUNC_INSTR(jmp1).int_val = CURR_IP() - jmp1;

			icode_gen_stmt(cx, stmt->child_2);
			FUNC_INSTR(jmp2).int_val = CURR_IP() - jmp2;
		}
		else
			FUNC_INSTR(jmp1).int_val = CURR_IP() - jmp1;
	}	break;

	case STMT_FOR: {
		usz it_size = type_bytes(stmt->sym->type);

		gen_sym_def(cx, stmt->sym, stmt->sym->expr);

		usz eval_ip = CURR_IP();

		ival_t end = icode_gen_expr(cx, stmt->expr);
		u32 end_reg = ival_reg(cx, it_size, end);

		u32 it_reg = ival_reg(cx, it_size, stmt->sym->val);

		u32 trg = alloc_reg(cx);
		usz jmp1 = emit(cx, ICODE(IR_IPO, ISZ_64, trg, .int_val = 0));
		emit(cx, ICODE3(IR_CJMPAE, it_size, trg, it_reg, end_reg));

		icode_gen_stmt(cx, stmt->child);

		u32 new_it = alloc_reg(cx);
		emit(cx, ICODE2(IR_INC, it_size, new_it, it_reg));
		emit(cx, ICODE2(IR_STOR, it_size, new_it, stmt->sym->val.reg));

		trg = alloc_reg(cx);
		emit(cx, ICODE(IR_IPO, ISZ_64, trg, .int_val = eval_ip - CURR_IP()));
		emit(cx, ICODE1(IR_JMP, ISZ_64, trg));

		FUNC_INSTR(jmp1).int_val = CURR_IP() - jmp1;
	}	break;

	default:
		lt_ferrf("Unhandled statement type '%S'\n", stmt_type_str(stmt->stype));
		break;
	}
}

void icode_gen(gen_ctx_t* cx, stmt_t* root) {
	stmt_t* it = root;
	while (it) {
		icode_gen_stmt(cx, it);
		it = it->next;
	}
}

