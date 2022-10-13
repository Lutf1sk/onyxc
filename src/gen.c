#include "interm.h"
#include "gen.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "type.h"
#include "symtab.h"
#include "segment.h"
#include "tk.h"
#include "err.h"
#include "textattrib.h"

#include <lt/io.h>
#include <lt/mem.h>
#include <lt/align.h>

#define ICODE_BLOCK_SIZE 512
#define ICODE_BLOCK_MASK (ICODE_BLOCK_SIZE-1)

static
usz emit(gen_ctx_t* cx, icode_t instr) {
	LT_ASSERT(cx->curr_func != -1);

	seg_ent_t* ent = &cx->segtab->seg[cx->curr_func];

	if (!(ent->icode_count & ICODE_BLOCK_MASK))
		ent->icode_data = realloc(ent->icode_data, (ent->icode_count + ICODE_BLOCK_SIZE) * sizeof(icode_t));

	((icode_t*)ent->icode_data)[ent->icode_count] = instr;
	return ent->icode_count++;
}

#define LBL_COUNT (cx->segtab->seg[cx->curr_func].lbls)

static
usz lalloc(gen_ctx_t* cx) {
	LT_ASSERT(cx->curr_func != -1);
	return ++LBL_COUNT;
}

static
void ldefine(gen_ctx_t* cx, usz lbl) {
	emit(cx, ICODE(IR_LBL, ISZ_64, 0, .uint_val = lbl));
}

#define REG_COUNT (cx->segtab->seg[cx->curr_func].regs)

static
u32 ralloc(gen_ctx_t* cx) {
	LT_ASSERT(cx->curr_func != -1);
	return ++REG_COUNT;
}

static
u32 immi_reg(gen_ctx_t* cx, usz size, u64 v) {
	u32 reg = ralloc(cx);
	emit(cx, ICODE(IR_INT, size, reg, .uint_val = v));
	return reg;
}

static
u32 ref_reg(gen_ctx_t* cx, usz size, u32 ref) {
	u32 reg = ralloc(cx);
	emit(cx, ICODE2(IR_LOAD, size, reg, ref));
	return reg;
}

static
u32 ival_reg(gen_ctx_t* cx, usz size, ival_t v) {
	switch (v.stype) {
	case IVAL_REG: return v.reg;
	case IVAL_IMM: return immi_reg(cx, size, v.uint_val);

	case IVAL_SEG: {
		u32 reg = ralloc(cx);
		emit(cx, ICODE2(IR_SEG, size, reg, v.uint_val));
		return reg;
	}

	case IVAL_SEG | IVAL_REF: {
		u32 ptr = ralloc(cx);
		emit(cx, ICODE2(IR_SEG, ISZ_64, ptr, v.uint_val));
		u32 reg = ralloc(cx);
		emit(cx, ICODE2(IR_LOAD, size, reg, ptr));
		return reg;
	}

	case IVAL_REG | IVAL_REF: return ref_reg(cx, size, v.reg);
	}

	*(u8*)0 = 0;
	LT_ASSERT_NOT_REACHED();
	return 0;
}

static
u32 ival_ptr(gen_ctx_t* cx, ival_t v) {
	switch (v.stype) {
	case IVAL_SEG:
	case IVAL_SEG | IVAL_REF: {
		u32 reg = ralloc(cx);
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
	case IVAL_SEG: return &cx->segtab->seg[v->uint_val].data;
	case IVAL_SEG | IVAL_REF: return cx->segtab->seg[v->uint_val].data;
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

u8* ival_write_comp(gen_ctx_t* cx, seg_ent_t* seg, type_t* type, ival_t v, u8* out) {
	if (v.stype != IVAL_COM) {
		void* ptr = NULL;
		usz size = type_bytes(type);

		switch (v.stype) {
		case IVAL_IMM: ptr = &v.uint_val; break;
		case IVAL_SEG | IVAL_REF: ptr = cx->segtab->seg[v.uint_val].data; break;

		case IVAL_SEG: {
			fwd_ref_t* new_ref = lt_amalloc(cx->arena, sizeof(fwd_ref_t));
			new_ref->offs = (usz)out - (usz)seg->data;
			new_ref->seg = v.uint_val;
			new_ref->size = size;
			new_ref->next = seg->ref;

			seg->ref = new_ref;
			memset(out, 0, size);
		}	return out + size;

		default: LT_ASSERT_NOT_REACHED(); break;
		}

		memcpy(out, ptr, size);
		return out + size;
	}

	if (type->stype == TP_STRUCT) {
		type_t** types = type->children;

		for (usz i = 0; i < v.child_count; ++i) {
			type_t* memb_type = types[i];
			ival_t memb_v = v.children[i];
			out = ival_write_comp(cx, seg, memb_type, memb_v, out);
		}
		return out;
	}
	else if (type->stype == TP_ARRAY_VIEW) {
		fwd_ref_t* new_ref = lt_amalloc(cx->arena, sizeof(fwd_ref_t));
		new_ref->offs = (usz)out - (usz)seg->data;
		new_ref->seg = v.children[0].uint_val;
		new_ref->size = ISZ_64;
		new_ref->next = seg->ref;

		seg->ref = new_ref;
		*(u64*)out = 0;
		out += sizeof(u64);

		*(u64*)out = v.children[1].uint_val;
		return out + sizeof(u64);
	}
	else if (type->stype == TP_ARRAY) {
		usz child_count = v.child_count;
		for (usz i = 0; i < child_count; ++i)
			out = ival_write_comp(cx, seg, type->base, v.children[i], out);
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
		emit(cx, ICODE2(IR_STOR, size, r1, src));
		return REG(src);
	}
}

static
u32 reg_add_immi(gen_ctx_t* cx, u32 reg, u64 v) {
	u32 new_reg = ralloc(cx);
	u32 immi = immi_reg(cx, ISZ_64, v);
	emit(cx, ICODE3(IR_ADD, ISZ_64, new_reg, reg, immi));
	return new_reg;
}

static
u32 ival_gen_comp(gen_ctx_t* cx, type_t* type, ival_t v, u32 reg) {
	if (v.stype != IVAL_COM) {
		usz size = type_bytes(type);
		gen_assign(cx, size, REF(reg), v);
		return reg_add_immi(cx, reg, size);
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
	u32 dst = ralloc(cx); \
	emit(cx, ICODE3(instr, size, dst, r1, r2)); \
	return REG(dst); \
}

OP_GEN(and, IR_AND, &)
OP_GEN(or, IR_OR, |)
OP_GEN(xor, IR_XOR, ^)

static
ival_t gen_negate(gen_ctx_t* cx, usz size, ival_t a) {
	if (a.stype == IVAL_IMM)
		return IMMI(-a.uint_val);
	u32 r = ival_reg(cx, size, a), dst = ralloc(cx);
	emit(cx, ICODE2(IR_NEG, size, dst, r));
	return REG(dst);
}

static
ival_t gen_add(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val + a2.uint_val);

	if (a1.stype == IVAL_IMM && a1.uint_val == 0)
		return a2;
	if (a2.stype == IVAL_IMM && a2.uint_val == 0)
		return a1;

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = ralloc(cx);
	emit(cx, ICODE3(IR_ADD, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_sub(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val - a2.uint_val);

	if (a1.stype == IVAL_IMM && a1.uint_val == 0)
		return gen_negate(cx, size, a2);
	if (a2.stype == IVAL_IMM && a2.uint_val == 0)
		return a1;

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = ralloc(cx);
	emit(cx, ICODE3(IR_SUB, size, dst, r1, r2));
	return REG(dst);
}

#define OP_GEN_SHIFT(name, instr, op, sign) \
static \
ival_t gen_##name(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) { \
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) \
		return IMMI(a1.sign##int_val op a2.sign##int_val); \
 \
	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, ISZ_8, a2); \
 \
	u32 dst = ralloc(cx); \
	emit(cx, ICODE3(instr, size, dst, r1, r2)); \
	return REG(dst); \
}

OP_GEN_SHIFT(ishl, IR_ISHL, <<, )
OP_GEN_SHIFT(ushl, IR_USHL, <<, u)
OP_GEN_SHIFT(ishr, IR_ISHR, >>, )
OP_GEN_SHIFT(ushr, IR_USHR, >>, u)

static
ival_t gen_imul(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.int_val * a2.int_val);

	if (a1.stype == IVAL_IMM) {
		switch (a1.uint_val) {
		case 0: return IMMI(0);
		case 1: return a2;
		}
	}
	if (a2.stype == IVAL_IMM) {
		switch (a2.uint_val) {
		case 0: return IMMI(0);
		case 1: return a1;
		}
	}

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = ralloc(cx);
	emit(cx, ICODE3(IR_IMUL, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_umul(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val * a2.uint_val);

	if (a1.stype == IVAL_IMM) {
		switch (a1.uint_val) {
		case 0: return IMMI(0);
		case 1: return a2;
		}
	}
	if (a2.stype == IVAL_IMM) {
		switch (a2.uint_val) {
		case 0: return IMMI(0);
		case 1: return a1;
		}
	}

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = ralloc(cx);
	emit(cx, ICODE3(IR_UMUL, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_idiv(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.int_val / a2.int_val);

	if (a1.stype == IVAL_IMM) {
		switch (a1.uint_val) {
//		case 0:
		case 1: return a2;
		}
	}
	if (a2.stype == IVAL_IMM) {
		switch (a2.uint_val) {
// 		case 0:
		case 1: return a1;
		}
	}

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = ralloc(cx);
	emit(cx, ICODE3(IR_IDIV, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_udiv(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val / a2.uint_val);

	if (a1.stype == IVAL_IMM) {
		switch (a1.uint_val) {
// 		case 0:
		case 1: return a2;
		}
	}
	if (a2.stype == IVAL_IMM) {
		switch (a2.uint_val) {
// 		case 0:
		case 1: return a1;
		}
	}

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = ralloc(cx);
	emit(cx, ICODE3(IR_UDIV, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_irem(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.int_val % a2.int_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = ralloc(cx);
	emit(cx, ICODE3(IR_IREM, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_urem(gen_ctx_t* cx, usz size, ival_t a1, ival_t a2) {
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
		return IMMI(a1.uint_val % a2.uint_val);

	u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);

	u32 dst = ralloc(cx);
	emit(cx, ICODE3(IR_UREM, size, dst, r1, r2));
	return REG(dst);
}

static
ival_t gen_static_compound(gen_ctx_t* cx, type_t* type, ival_t* v) {
	usz size = type_bytes(type);
	void* data = lt_amalloc(cx->arena, size);
	memset(data, 0, size);

	u32 seg_i = new_segment(cx->segtab, DATASEG(CLSTR("@STATCOM"), size, data));
	ival_write_comp(cx, &cx->segtab->seg[seg_i], type, *v, data);
	return IVAL(IVAL_SEG | IVAL_REF, .uint_val = seg_i);
}

static
ival_t gen_stack_compound(gen_ctx_t* cx, type_t* type, ival_t* v) {
	u32 reg = ralloc(cx);
	emit(cx, ICODE3(IR_SRESV, ISZ_64, reg, type_bytes(type), type_align(type)));
	ival_gen_comp(cx, type, *v, reg);
	return REF(reg);
}

static
void gen_sym_def(gen_ctx_t* cx, sym_t* sym, expr_t* expr) {
	if (sym->flags & SYMFL_CONST)
		return;

	if (!(sym->flags & SYMFL_GLOBAL)) {
		usz size = type_bytes(sym->type);
		usz align = type_align(sym->type);

		if (expr) {
			ival_t val = icode_gen_expr(cx, expr);
			u32 reg = ralloc(cx);
			sym->val = REF(reg);
			emit(cx, ICODE3(IR_SRESV, ISZ_64, reg, size, align));

			if (val.stype == IVAL_COM)
				val = gen_static_compound(cx, sym->type, &val);

			gen_assign(cx, size, sym->val, val);
		}
		else {
			sym->val = REF(ralloc(cx));
			emit(cx, ICODE3(IR_SRESV, ISZ_64, sym->val.reg, size, align));
		}
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
		usz new_func = new_segment(cx->segtab, CODESEG(CLSTR("@LAMDA"), expr->type));
		cx->curr_func = new_func;

		cx->segtab->seg[new_func].label_symtab = expr->label_symtab;
		cx->segtab->seg[new_func].lbls = expr->label_symtab->count;

		emit(cx, ICODE0(IR_ENTER, ISZ_64));
		sym_t** args = expr->type->child_syms;
		isz arg_count = expr->type->child_count;
		usz stack_size = 0;
		for (isz i = arg_count - 1; i >= 0; stack_size += type_bytes(expr->type->children[i--])) {
			sym_t* sym = args[i];
// 			if (!sym /*|| !(sym->flags & SYMFL_ACCESSED)*/)
// 				continue;
			gen_sym_def(cx, sym, NULL);
			emit(cx, ICODE2(IR_GETARG, type_bytes(sym->type), sym->val.reg, i));
		}

		icode_gen_stmt(cx, expr->stmt);

		// TODO: Insert a ret only when not all code paths return
		emit(cx, ICODE0(IR_RET, 0));

		cx->curr_func = old_func;
		return SEG(new_func);
	}

	case EXPR_STRUCT: {
		usz max_children = expr->type->child_count;
		ival_t* children = lt_amalloc(cx->arena, max_children * sizeof(ival_t));

		expr_t* it = expr->child_1;
		usz i = 0;
		for (; i < max_children && it; ++i) {
			children[i] = gen_const_expr(cx, it);
			it = it->next;
		}

		return IVAL(IVAL_COM, i, .children = children);
	}

	case EXPR_VIEW: {
		LT_ASSERT(!expr->child_2); // !!

		usz child_count = 2;
		ival_t* children = lt_amalloc(cx->arena, child_count * sizeof(ival_t));
		ival_t addr = gen_const_expr(cx, expr->child_1);
		if (addr.stype == IVAL_COM)
			addr = gen_static_compound(cx, expr->child_1->type, &addr);
 		LT_ASSERT(addr.stype == (IVAL_SEG | IVAL_REF));

		children[0] = SEG(addr.uint_val);
		children[1] = IMMI(expr->child_1->type->child_count);
		return IVAL(IVAL_COM, child_count, .children = children);
	}

	case EXPR_ARRAY: {
		usz count = expr->type->child_count;
		ival_t* children = lt_amalloc(cx->arena, count * sizeof(ival_t));

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
		ival_t* children = lt_amalloc(cx->arena, count * sizeof(ival_t));
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

static
ival_t gen_ptr_view(gen_ctx_t* cx, ival_t v, ival_t start_index, usz elem_size, ival_t count) {
	ival_t ptr = gen_add(cx, ISZ_64, v, gen_imul(cx, ISZ_64, start_index, IMMI(elem_size)));

	u32 view_start = ralloc(cx);
	emit(cx, ICODE3(IR_SRESV, ISZ_64, view_start, 16, 8));

	gen_assign(cx, ISZ_64, REF(view_start), ptr);
	u32 count_ptr = ival_reg(cx, ISZ_64, gen_add(cx, ISZ_64, REG(view_start), IMMI(8)));
	gen_assign(cx, ISZ_64, REF(count_ptr), count);

	return REF(view_start);
}

static
ival_t gen_view_data(gen_ctx_t* cx, ival_t v) {
	if (v.stype == IVAL_COM)
		return v.children[0];
	return v;
}

static
ival_t gen_view_count(gen_ctx_t* cx, ival_t v) {
	if (v.stype == IVAL_COM)
		return v.children[1];

	u32 reg = ival_ptr(cx, v);
	ival_t ref = gen_add(cx, ISZ_64, REG(reg), IMMI(8));
	ref.stype |= IVAL_REF;
	return ref;
}

ival_t icode_gen_expr(gen_ctx_t* cx, expr_t* expr) {
	LT_ASSERT(expr->type);

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
// 		ival_t ptr, count;
		if (expr->child_2) {
			LT_ASSERT(expr->child_2->next);
			return gen_ptr_view(cx, icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2), type_bytes(expr->child_1->type), icode_gen_expr(cx, expr->child_2->next));
		}
		else {
			ival_t v = icode_gen_expr(cx, expr->child_1);
			if (v.stype == IVAL_COM)
				v = gen_static_compound(cx, expr->child_1->type, &v);

			LT_ASSERT(v.stype & IVAL_REF);
			v.stype &= ~IVAL_REF;

// 			count = IMMI(expr->child_1->type->child_count);
			return gen_ptr_view(cx, v, IMMI(0), 0, IMMI(expr->child_1->type->child_count));
		}
// 		u32 view_start = ralloc(cx);
// 		emit(cx, ICODE3(IR_SRESV, ISZ_64, view_start, 16, 8));

// 		gen_assign(cx, ISZ_64, REF(view_start), ptr);
// 		u32 count_ptr = ival_reg(cx, ISZ_64, gen_add(cx, ISZ_64, REG(view_start), IMMI(8)));
// 		gen_assign(cx, ISZ_64, REF(count_ptr), count);

// 		return REF(view_start);
	}

	case EXPR_ARRAY: {
		usz count = expr->type->child_count;
		usz elem_size = type_bytes(expr->type->base);
		usz size = count * elem_size;
		LT_ASSERT(cx->curr_func != -1);

		usz arr_start = ralloc(cx);
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

		u32 struct_start = ralloc(cx);
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
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			return gen_imul(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_umul(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_DIVIDE:
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			return gen_idiv(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_udiv(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_MODULO:
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
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
		if (is_int(resolve_enum(expr->child_1->type)))
			return gen_ishl(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_ushl(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_BIT_SHIFT_RIGHT:
		if (is_int(resolve_enum(expr->child_1->type)))
			return gen_ishr(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));
		else
			return gen_ushr(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1), icode_gen_expr(cx, expr->child_2));

	case EXPR_CONVERT: {
		usz to = type_bytes(expr->type), from = type_bytes(expr->child_1->type);
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		if (a1.stype == IVAL_IMM || a1.stype == IVAL_SEG)
			return a1;

		if (expr->type->stype == TP_ARRAY_VIEW && expr->child_1->type->stype == TP_ARRAY_VIEW) {\
			if (type_eq(resolve_enum(expr->type), &void_view_def)) {
				usz elem_size = type_bytes(expr->child_1->type->base);
				ival_t count_bytes = IMMI(0);
				if (elem_size)
					count_bytes = gen_umul(cx, ISZ_64, gen_view_count(cx, a1), IMMI(elem_size));
				return gen_ptr_view(cx, gen_view_data(cx, a1), IMMI(0), elem_size, count_bytes);
			}
			else if (type_eq(resolve_enum(expr->child_1->type), &void_view_def)) {
				usz elem_size = type_bytes(expr->type->base);
				ival_t count_bytes = IMMI(0);
				if (elem_size)
					count_bytes = gen_udiv(cx, ISZ_64, gen_view_count(cx, a1), IMMI(elem_size));
				return gen_ptr_view(cx, gen_view_data(cx, a1), IMMI(0), elem_size, count_bytes);
			}
		}

		u32 op = IR_INVAL;
		if (is_int(resolve_enum(expr->type)) && is_int(resolve_enum(expr->child_1->type))) {
			switch (to) {
			case ISZ_8: op = IR_TOI8; break;
			case ISZ_16: op = IR_TOI16; break;
			case ISZ_32: op = IR_TOI32; break;
			case ISZ_64: op = IR_TOI64; break;
			}
		}
		else if (is_number(resolve_enum(expr->type)) && is_number(resolve_enum(expr->child_1->type))) {
			switch (to) {
			case ISZ_8: op = IR_TOU8; break;
			case ISZ_16: op = IR_TOU16; break;
			case ISZ_32: op = IR_TOU32; break;
			case ISZ_64: op = IR_TOU64; break;
			}
		}
		else
			LT_ASSERT_NOT_REACHED();
		u32 r1 = ival_reg(cx, from, a1), dst = ralloc(cx);
		emit(cx, ICODE2(op, from, dst, r1));
		return REG(dst);
	}

	// Unary
	case EXPR_NEGATE: return gen_negate(cx, type_bytes(expr->type), icode_gen_expr(cx, expr->child_1));

	case EXPR_PFX_INCREMENT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		usz size = type_bytes(expr->type);

		u32 ptr = ival_ptr(cx, a);
		u32 dst = ralloc(cx), val = ref_reg(cx, size, ptr);

		emit(cx, ICODE2(IR_INC, size, dst, val));
		emit(cx, ICODE2(IR_STOR, size, ptr, dst));
		return REG(dst);
	}

	case EXPR_PFX_DECREMENT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		usz size = type_bytes(expr->type);

		u32 ptr = ival_ptr(cx, a);
		u32 dst = ralloc(cx), val = ref_reg(cx, size, ptr);

		emit(cx, ICODE2(IR_DEC, size, dst, val));
		emit(cx, ICODE2(IR_STOR, size, ptr, dst));
		return REG(dst);
	}

	case EXPR_SFX_INCREMENT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		usz size = type_bytes(expr->type);

		u32 ptr = ival_ptr(cx, a);
		u32 tmp = ralloc(cx), val = ref_reg(cx, size, ptr);

		emit(cx, ICODE2(IR_INC, size, tmp, val));
		emit(cx, ICODE2(IR_STOR, size, ptr, tmp));
		return REG(val);
	}

	case EXPR_SFX_DECREMENT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		check_lval(cx, expr->tk, a);
		usz size = type_bytes(expr->type);

		u32 ptr = ival_ptr(cx, a);
		u32 tmp = ralloc(cx), val = ref_reg(cx, size, ptr);

		emit(cx, ICODE2(IR_DEC, size, tmp, val));
		emit(cx, ICODE2(IR_STOR, size, ptr, tmp));
		return REG(val);
	}

	case EXPR_BIT_NOT: {
		ival_t a = icode_gen_expr(cx, expr->child_1);
		if (a.stype == IVAL_IMM)
			return IMMI(!a.uint_val);
		usz size = type_bytes(expr->type);
		u32 r = ival_reg(cx, size, a), dst = ralloc(cx);
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
			u32 reg = ralloc(cx);
			emit(cx, ICODE3(IR_SRESV, ISZ_64, reg, ret_size, ret_align));
			ret_val = REF(reg);
		}
		else
			ret_val = REG(ralloc(cx));

		usz arg_count = expr->child_1->type->child_count;
		u32* arg_regs = lt_amalloc(cx->arena, arg_count * sizeof(u32));
		usz* arg_sizes = lt_amalloc(cx->arena, arg_count * sizeof(usz));

		expr_t* it = expr->child_2;
		for (usz i = 0; it; ++i, it = it->next) {
			usz size = type_bytes(it->type);
			u32 reg;
			arg_sizes[i] = size;
			ival_t ival = icode_gen_expr(cx, it);
			if (size > ISZ_64) {
				if (ival.stype == IVAL_COM)
					ival = gen_static_compound(cx, it->type, &ival);
				reg = ival_ptr(cx, ival);
			}
			else
				reg = ival_reg(cx, size, ival);
			arg_regs[i] = reg;
		}

		u32 func_addr = ival_reg(cx, ISZ_64, icode_gen_expr(cx, expr->child_1));

		emit(cx, ICODE1(IR_BEGCALL, ret_size, 0/*CCONV_SYSV*/));
		for (usz i = 0; i < arg_count; ++i)
			emit(cx, ICODE2(IR_SETARG, arg_sizes[i], arg_regs[i], i));

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
		if (a1.stype == IVAL_COM)
			a1 = gen_static_compound(cx, expr->child_1->type, &a1);
		if (a2.stype == IVAL_COM)
			a2 = gen_static_compound(cx, expr->child_2->type, &a2);
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
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			OP_ASSIGN(imul)
		else
			OP_ASSIGN(umul)
	case EXPR_DIVIDE_ASSIGN:
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			OP_ASSIGN(idiv)
		else
			OP_ASSIGN(udiv)
	case EXPR_MODULO_ASSIGN:
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			OP_ASSIGN(irem)
		else
			OP_ASSIGN(urem)
	case EXPR_BIT_SHIFT_LEFT_ASSIGN:
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			OP_ASSIGN(ishl)
		else
			OP_ASSIGN(ushl)
	case EXPR_BIT_SHIFT_RIGHT_ASSIGN:
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
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
		u32 zero = immi_reg(cx, size, 0);
		u32 one = immi_reg(cx, size, 1);

		u32 dst = ralloc(cx);
		emit(cx, ICODE3(IR_SRESV, ISZ_64, dst, size, type_align(expr->type)));
		emit(cx, ICODE2(IR_STOR, size, dst, zero));

		usz false_lbl = lalloc(cx);
		u32 false_reg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, false_reg, .int_val = false_lbl));

		usz size1 = type_bytes(expr->child_1->type);
		u32 r1 = ival_reg(cx, size1, icode_gen_expr(cx, expr->child_1));
		emit(cx, ICODE2(IR_CJMPZ, size1, false_reg, r1));

		usz size2 = type_bytes(expr->child_2->type);
		u32 r2 = ival_reg(cx, size2, icode_gen_expr(cx, expr->child_2));
		emit(cx, ICODE2(IR_CJMPZ, size2, false_reg, r2));

		emit(cx, ICODE2(IR_STOR, size, dst, one));

		ldefine(cx, false_lbl);
		return REF(dst);
	}

	case EXPR_LOGIC_OR: {
		usz size = type_bytes(expr->type);
		u32 zero = immi_reg(cx, size, 0);
		u32 one = immi_reg(cx, size, 1);

		u32 dst = immi_reg(cx, size, 1);
		emit(cx, ICODE3(IR_SRESV, ISZ_64, dst, size, type_align(expr->type)));
		emit(cx, ICODE2(IR_STOR, size, dst, one));

		usz true_lbl = lalloc(cx);
		u32 true_reg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, true_reg, .int_val = true_lbl));

		usz size1 = type_bytes(expr->child_1->type);
		u32 r1 = ival_reg(cx, size1, icode_gen_expr(cx, expr->child_1));
		emit(cx, ICODE2(IR_CJMPNZ, size1, true_reg, r1));

		usz size2 = type_bytes(expr->child_2->type);
		u32 r2 = ival_reg(cx, size2, icode_gen_expr(cx, expr->child_2));
		emit(cx, ICODE2(IR_CJMPNZ, size2, true_reg, r2));

		emit(cx, ICODE2(IR_STOR, size, dst, zero));

		ldefine(cx, true_lbl);
		return REF(dst);
	}

	case EXPR_LOGIC_NOT: {
		usz size = type_bytes(expr->child_1->type);
		u32 r = ival_reg(cx, size, icode_gen_expr(cx, expr->child_1));
		u32 dst = ralloc(cx);
		emit(cx, ICODE2(IR_CSETZ, size, dst, r));
		return REG(dst);
	}

	case EXPR_LESSER: {
		usz size = type_bytes(expr->child_1->type);
		u32 dst = ralloc(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) {
			if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
				return IMMI(a1.int_val < a2.int_val);
			else
				return IMMI(a1.uint_val < a2.uint_val);
		}

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			emit(cx, ICODE3(IR_CSETL, size, dst, r1, r2));
		else
			emit(cx, ICODE3(IR_CSETB, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_GREATER: {
		usz size = type_bytes(expr->child_1->type);
		u32 dst = ralloc(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) {
			if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
				return IMMI(a1.int_val > a2.int_val);
			else
				return IMMI(a1.uint_val > a2.uint_val);
		}

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			emit(cx, ICODE3(IR_CSETG, size, dst, r1, r2));
		else
			emit(cx, ICODE3(IR_CSETA, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_LESSER_OR_EQUAL: {
		usz size = type_bytes(expr->child_1->type);
		u32 dst = ralloc(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) {
			if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
				return IMMI(a1.int_val <= a2.int_val);
			else
				return IMMI(a1.uint_val <= a2.uint_val);
		}

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			emit(cx, ICODE3(IR_CSETLE, size, dst, r1, r2));
		else
			emit(cx, ICODE3(IR_CSETBE, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_GREATER_OR_EQUAL: {
		usz size = type_bytes(expr->child_1->type);
		u32 dst = ralloc(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) {
			if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
				return IMMI(a1.int_val >= a2.int_val);
			else
				return IMMI(a1.uint_val >= a2.uint_val);
		}

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		if (is_int(resolve_enum(expr->child_1->type)) && is_int(resolve_enum(expr->child_2->type)))
			emit(cx, ICODE3(IR_CSETGE, size, dst, r1, r2));
		else
			emit(cx, ICODE3(IR_CSETAE, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_EQUAL: {
		usz size = type_bytes(expr->child_1->type);
		u32 dst = ralloc(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IMMI(a1.uint_val == a2.uint_val);

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		emit(cx, ICODE3(IR_CSETE, size, dst, r1, r2));
		return REG(dst);
	}

	case EXPR_NOT_EQUAL: {
		usz size = type_bytes(expr->child_1->type);
		u32 dst = ralloc(cx);
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IMMI(a1.uint_val != a2.uint_val);

		u32 r1 = ival_reg(cx, size, a1), r2 = ival_reg(cx, size, a2);
		emit(cx, ICODE3(IR_CSETNE, size, dst, r1, r2));
		return REG(dst);
	}

	// Misc.
	case EXPR_SYSCALL: {
		u32* arg_regs = lt_amalloc(cx->arena, 8 * sizeof(u32));

		expr_t* it = expr->child_1;
		usz argc = 0;
		while (it) {
			arg_regs[argc++] = ival_reg(cx, ISZ_64, icode_gen_expr(cx, it));
			it = it->next;
		}

		emit(cx, ICODE1(IR_BEGCALL, ISZ_64, 1/*CCONV_SYSCALL*/));
		for (usz i = 0; i < argc; ++i)
			emit(cx, ICODE2(IR_SETARG, ISZ_64, arg_regs[i], i));

		u32 dst = ralloc(cx);
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
	case STMT_SYMDEF:
		if (stmt->sym->stype == SYM_VAR)
			gen_sym_def(cx, stmt->sym, stmt->expr);
		break;

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

	case STMT_WHILE: {
		usz size = type_bytes(stmt->expr->type);
		usz end_lbl = lalloc(cx), start_lbl = lalloc(cx);

		usz old_break = cx->break_lbl;
		usz old_cont = cx->cont_lbl;
		cx->break_lbl = end_lbl;
		cx->cont_lbl = start_lbl;

		ldefine(cx, start_lbl);

		ival_t cond_v = icode_gen_expr(cx, stmt->expr);
		u32 cond_reg = ival_reg(cx, size, cond_v);
		u32 trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .int_val = end_lbl));

		emit(cx, ICODE2(IR_CJMPZ, size, trg, cond_reg));

		icode_gen_stmt(cx, stmt->child);

		trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .int_val = start_lbl));
		emit(cx, ICODE1(IR_JMP, ISZ_64, trg));

		ldefine(cx, end_lbl);

		cx->break_lbl = old_break;
		cx->cont_lbl = old_cont;
	}	break;

	case STMT_DO: {
		usz size = type_bytes(stmt->expr->type);
		usz start_lbl = lalloc(cx), cont_lbl = lalloc(cx), break_lbl = lalloc(cx);

		usz old_break = cx->break_lbl;
		usz old_cont = cx->cont_lbl;
		cx->break_lbl = cont_lbl;
		cx->cont_lbl = break_lbl;

		ldefine(cx, start_lbl);

		icode_gen_stmt(cx, stmt->child);

		ldefine(cx, cont_lbl);

		ival_t cond_v = icode_gen_expr(cx, stmt->expr);
		u32 cond_reg = ival_reg(cx, size, cond_v);
		u32 trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .int_val = start_lbl));
		emit(cx, ICODE2(IR_CJMPNZ, size, trg, cond_reg));

		ldefine(cx, break_lbl);

		cx->break_lbl = old_break;
		cx->cont_lbl = old_cont;
	}	break;

	case STMT_IF: {
		usz size = type_bytes(stmt->expr->type);
		usz false_lbl = lalloc(cx);

		ival_t cond_v = icode_gen_expr(cx, stmt->expr);
		u32 cond_reg = ival_reg(cx, size, cond_v);
		u32 trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = false_lbl));
		emit(cx, ICODE2(IR_CJMPZ, size, trg, cond_reg));
		icode_gen_stmt(cx, stmt->child);

		if (stmt->child_2) {
			usz true_lbl = lalloc(cx);
			trg = ralloc(cx);
			emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = true_lbl));
			emit(cx, ICODE1(IR_JMP, ISZ_64, trg));

			ldefine(cx, false_lbl);
			icode_gen_stmt(cx, stmt->child_2);
			ldefine(cx, true_lbl);
		}
		else
			ldefine(cx, false_lbl);
	}	break;

	case STMT_FOR: {
		usz end_lbl = lalloc(cx), start_lbl = lalloc(cx);

		usz old_break = cx->break_lbl;
		usz old_cont = cx->cont_lbl;
		cx->break_lbl = end_lbl;
		cx->cont_lbl = start_lbl;

		usz it_size = type_bytes(stmt->sym->type);

		gen_sym_def(cx, stmt->sym, stmt->sym->expr);

		ldefine(cx, start_lbl);
		ival_t end = icode_gen_expr(cx, stmt->expr);
		u32 end_reg = ival_reg(cx, it_size, end);

		u32 it_reg = ival_reg(cx, it_size, stmt->sym->val);

		u32 trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = end_lbl));
		emit(cx, ICODE3(IR_CJMPAE, it_size, trg, it_reg, end_reg));

		icode_gen_stmt(cx, stmt->child);

		u32 new_it = ralloc(cx);
		emit(cx, ICODE2(IR_INC, it_size, new_it, it_reg));
		emit(cx, ICODE2(IR_STOR, it_size, stmt->sym->val.reg, new_it));

		trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = start_lbl));
		emit(cx, ICODE1(IR_JMP, ISZ_64, trg));

		ldefine(cx, end_lbl);

		cx->break_lbl = old_break;
		cx->cont_lbl = old_cont;
	}	break;

	case STMT_BREAK: {
		if (!cx->break_lbl)
			ferr("break must only be used inside of a loop", *stmt->tk);
		u32 trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = cx->break_lbl));
		emit(cx, ICODE1(IR_JMP, ISZ_64, trg));
	}	break;

	case STMT_CONTINUE: {
		if (!cx->cont_lbl)
			ferr("continue must only be used inside of a loop", *stmt->tk);
		u32 trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = cx->cont_lbl));
		emit(cx, ICODE1(IR_JMP, ISZ_64, trg));
	}	break;

	case STMT_LABEL:
		ldefine(cx, stmt->sym->lbl);
		break;

	case STMT_GOTO: {
		sym_t* sym = symtab_find(cx->segtab->seg[cx->curr_func].label_symtab, stmt->tk->str);
		if (!sym)
			ferr(A_BOLD"'%S'"A_RESET" is not a valid label", *stmt->tk, stmt->tk->str);

		u32 trg = ralloc(cx);
		emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = sym->lbl));
		emit(cx, ICODE1(IR_JMP, ISZ_64, trg));
	}	break;

	case STMT_SWITCH: {
		u32 cond = ival_reg(cx, type_bytes(stmt->expr->type), icode_gen_expr(cx, stmt->expr));
		usz lbl_end = lalloc(cx);

		usz* case_lbls = lt_amalloc(cx->arena, sizeof(usz) * 64); // !!

		stmt_t* default_ = NULL;

		// TODO: check for duplicate cases

		usz case_count = 0;
		for (stmt_t* case_it = stmt->child; case_it; case_it = case_it->next) {
			if (case_it->stype == STMT_DEFAULT) {
				default_ = case_it;
				continue;
			}

			usz lbl_case = lalloc(cx);

			for (expr_t* expr_it = case_it->expr; expr_it; expr_it = expr_it->next) {
				u32 case_reg = ival_reg(cx, type_bytes(expr_it->type), gen_const_expr(cx, expr_it));

				u32 trg = ralloc(cx);
				emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = lbl_case));
				emit(cx, ICODE3(IR_CJMPE, ISZ_64, trg, cond, case_reg));
			}

			case_lbls[case_count++] = lbl_case;
		}

		if (default_) {
			icode_gen_stmt(cx, default_->child);

			u32 trg = ralloc(cx);
			emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = lbl_end));
			emit(cx, ICODE1(IR_JMP, ISZ_64, trg));
		}

		case_count = 0;
		for (stmt_t* case_it = stmt->child; case_it; case_it = case_it->next) {
			if (case_it->stype == STMT_DEFAULT)
				continue;

			ldefine(cx, case_lbls[case_count++]);
			icode_gen_stmt(cx, case_it->child);

			u32 trg = ralloc(cx);
			emit(cx, ICODE(IR_GETLBL, ISZ_64, trg, .uint_val = lbl_end));
			emit(cx, ICODE1(IR_JMP, ISZ_64, trg));
		}

		ldefine(cx, lbl_end);
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

