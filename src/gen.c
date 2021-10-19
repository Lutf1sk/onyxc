#include <lt/mem.h>

#include "interm.h"
#include "gen.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "type.h"
#include "symtab.h"
#include "segment.h"
#include "tk.h"

#include <lt/io.h>

#define ICODE_BLOCK_SIZE 1024
#define ICODE_BLOCK_MASK (ICODE_BLOCK_SIZE-1)

#define SEGMENT_BLOCK_SIZE 1024
#define SEGMENT_BLOCK_MASK (ICODE_BLOCK_SIZE-1)

static
usz emit(gen_ctx_t* cx, icode_t instr) {
	LT_ASSERT(cx->curr_func != -1);

	seg_ent_t* ent = &cx->code_seg[cx->curr_func];

	if (!(ent->size & ICODE_BLOCK_MASK))
		ent->data = realloc(ent->data, (ent->size + ICODE_BLOCK_SIZE) * sizeof(icode_t));

	((icode_t*)ent->data)[ent->size] = instr;
	return ent->size++;
}

static
usz new_code_seg(gen_ctx_t* cx, type_t* type) {
	if (!(cx->code_seg_count & SEGMENT_BLOCK_MASK))
		cx->code_seg = realloc(cx->code_seg, (cx->code_seg_count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	memset(&cx->code_seg[cx->code_seg_count], 0, sizeof(seg_ent_t));
	cx->code_seg[cx->code_seg_count].type = type;
	return cx->code_seg_count++;
}

static
usz new_data_seg(gen_ctx_t* cx, seg_ent_t new_ent) {
	if (!(cx->data_seg_count & SEGMENT_BLOCK_MASK))
		cx->data_seg = realloc(cx->data_seg, (cx->data_seg_count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	cx->data_seg[cx->data_seg_count] = new_ent;
	return cx->data_seg_count++;
}

static usz reg__ = 0;

static
ival_t gen_offset_ref(gen_ctx_t* cx, ival_t ref, ival_t offs) {
	LT_ASSERT(ref.stype & IVAL_REF);

	switch (ref.stype & ~IVAL_REF) {
	case IVAL_IMM:
		if (offs.stype == IVAL_IMM) {
			ref.uint_val += offs.uint_val;
			return ref;
		}

	case IVAL_REG: {
		if (offs.stype == IVAL_IMM) {
			if (offs.uint_val == 0)
				return ref;
		}

		usz size = ref.size;

		ival_t dst = IVAL(ISZ_64, IVAL_REG, .reg = reg__++);
		ref.stype &= ~IVAL_REF;
		ref.size = ISZ_64;

		emit(cx, ICODE(IR_ADD, dst, ref, offs));
		dst.size = size;
		dst.stype |= IVAL_REF;
		return dst;
	}

	case IVAL_SFO:
		if (offs.stype == IVAL_IMM) {
			ref.uint_val += offs.uint_val;
			return ref;
		}
	case IVAL_DSO: case IVAL_CSO: {
		ival_t dst = IVAL(ISZ_64, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_LEA, dst, ref, IVAL(0, 0)));

		ival_t dst2 = IVAL(ISZ_64, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_ADD, dst2, dst, offs));
		dst2.stype |= IVAL_REF;
		dst2.size = ref.size;
		return dst2;
	}
	}

	LT_ASSERT_NOT_REACHED();
	return IVAL(0, 0);
}

static
void gen_sym_def(gen_ctx_t* cx, sym_t* sym, expr_t* expr) {
	if (sym->flags & SYMFL_CONST)
		sym->ival = icode_gen_expr(cx, expr);
	else {
		if (sym->flags & SYMFL_GLOBAL) {
			usz size = type_bytes(sym->type);
			void* data = lt_arena_reserve(cx->arena, size);

			memset(data, 0, size);
			if (expr) {
				icode_gen_expr(cx, expr);
			}

			usz offs = new_data_seg(cx, SEG_ENT(sym->name, size, data));
			sym->ival = IVAL(size, IVAL_DSO | IVAL_REF, .uint_val = offs);
		}
		else {
			u8 flags = sym->flags;
			if (!(flags & SYMFL_ACCESSED))
				return;

			usz size = type_bytes(sym->type);
			ival_t val;
			b8 scalar = is_scalar(sym->type);

			usz sf_offs = cx->code_seg[cx->curr_func].top;
			if (!scalar) {
				val = IVAL(size, IVAL_SFO | IVAL_REF, .uint_val = sf_offs);
				if (expr) {
					ival_t init = icode_gen_expr(cx, expr);
					emit(cx, ICODE(IR_COPY, val, init, IVAL(0, 0)));
				}
				cx->code_seg[cx->curr_func].top += size;
			}
			else if (flags & SYMFL_REFERENCED) {
				val = IVAL(size, IVAL_SFO | IVAL_REF, .uint_val = sf_offs);
				emit(cx, ICODE2(IR_MOV, val, icode_gen_expr(cx, expr)));
				cx->code_seg[cx->curr_func].top += size;
			}
			else {
				ival_t dst = IVAL(size, IVAL_REG, .reg = reg__++);
				if (expr)
					emit(cx, ICODE(IR_MOV, dst, icode_gen_expr(cx, expr), IVAL(0, 0)));
				val = dst;
			}

			sym->ival = val;
		}
	}
}

#define GENERIC_EXPR_BINARY(icode, operator, sign) { \
	ival_t a1 = icode_gen_expr(cx, expr->child_1); \
	ival_t a2 = icode_gen_expr(cx, expr->child_2); \
	if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM) \
		return IVAL(a1.size, IVAL_IMM, .uint_val = a1.sign##int_val operator a2.sign##int_val); \
	ival_t dst = IVAL(type_bytes(expr->type), IVAL_REG, .reg = reg__++); \
	emit(cx, ICODE((icode), dst, a1, a2)); \
	return dst; \
}

#define GENERIC_EXPR_UNARY(icode, operator) { \
	ival_t a1 = icode_gen_expr(cx, expr->child_1); \
	if (a1.stype == IVAL_IMM) \
		return IVAL(a1.size, IVAL_IMM, .uint_val = operator a1.int_val); \
	ival_t dst = IVAL(type_bytes(expr->type), IVAL_REG, .reg = reg__++); \
	emit(cx, ICODE((icode), dst, a1, IVAL(0, 0))); \
	return dst; \
}

#define FUNC_INSTR(x) (((icode_t*)cx->code_seg[cx->curr_func].data)[(x)])
#define CURR_INSTR() (cx->code_seg[cx->curr_func].size)

ival_t icode_gen_expr(gen_ctx_t* cx, expr_t* expr) {
	switch (expr->stype) {
	case EXPR_ADD:
		GENERIC_EXPR_BINARY(IR_ADD, +, u);

	case EXPR_SUBTRACT:
		GENERIC_EXPR_BINARY(IR_SUB, -, u);

	case EXPR_MULTIPLY:
		if (is_int(expr->type))
			GENERIC_EXPR_BINARY(IR_IMUL, *, )
		else if (is_uint(expr->type))
			GENERIC_EXPR_BINARY(IR_UMUL, *, u)

	case EXPR_DIVIDE:
		if (is_int(expr->type))
			GENERIC_EXPR_BINARY(IR_IDIV, /, )
		else if (is_uint(expr->type))
			GENERIC_EXPR_BINARY(IR_UDIV, /, u)

	case EXPR_MODULO:
		if (is_int(expr->type))
			GENERIC_EXPR_BINARY(IR_IREM, %, )
		else if (is_uint(expr->type))
			GENERIC_EXPR_BINARY(IR_UREM, %, u)

	case EXPR_NEGATE:
		GENERIC_EXPR_UNARY(IR_NEG, -);

	case EXPR_PFX_INCREMENT: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		if (a1.stype == IVAL_IMM)
			return IVAL(a1.size, IVAL_IMM, .uint_val = a1.int_val + 1);

		emit(cx, ICODE1(IR_INC, a1));
		return a1;
	}

	case EXPR_PFX_DECREMENT: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		if (a1.stype == IVAL_IMM)
			return IVAL(a1.size, IVAL_IMM, .uint_val = a1.int_val - 1);

		emit(cx, ICODE1(IR_DEC, a1));
		return a1;
	}

	case EXPR_SFX_INCREMENT:
		GENERIC_EXPR_UNARY(IR_INCSFX, );

	case EXPR_SFX_DECREMENT:
		GENERIC_EXPR_UNARY(IR_DECSFX, );

	case EXPR_BIT_AND:
		GENERIC_EXPR_BINARY(IR_AND, &, u);

	case EXPR_BIT_OR:
		GENERIC_EXPR_BINARY(IR_OR, |, u);

	case EXPR_BIT_XOR:
		GENERIC_EXPR_BINARY(IR_XOR, ^, u);

	case EXPR_BIT_SHIFT_LEFT:
		if (is_int(expr->type))
			GENERIC_EXPR_BINARY(IR_ISHL, >>, )
		else if (is_uint(expr->type))
			GENERIC_EXPR_BINARY(IR_USHL, >>, u)

	case EXPR_BIT_SHIFT_RIGHT:
		if (is_int(expr->type))
			GENERIC_EXPR_BINARY(IR_ISHR, >>, )
		else if (is_uint(expr->type))
			GENERIC_EXPR_BINARY(IR_USHR, >>, u)

	case EXPR_BIT_NOT:
		GENERIC_EXPR_UNARY(IR_NOT, ~);

	case EXPR_ASSIGN: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		ival_t a2 = icode_gen_expr(cx, expr->child_2);
		LT_ASSERT(a1.stype & IVAL_REF || a1.stype == IVAL_REG);
		emit(cx, ICODE(IR_MOV, a1, a2, IVAL(0, 0)));
		return a1;
	}

	case EXPR_LAMBDA:
		isz old_func = cx->curr_func, new_func = new_code_seg(cx, expr->type);
		cx->curr_func = new_func;

		usz enter = emit(cx, ICODE1(IR_ENTER, IVAL(ISZ_64, IVAL_IMM, .uint_val = 0)));
		sym_t** args = expr->type->child_syms;
		isz arg_count = expr->type->child_count;
		usz stack_size = 0;
		for (isz i = arg_count - 1; i >= 0; stack_size += type_bytes(expr->type->children[i--])) {
			sym_t* sym = args[i];
			if (!sym || !(sym->flags & SYMFL_ACCESSED))
				continue;
			gen_sym_def(cx, sym, NULL);
			emit(cx, ICODE2(IR_GETARG, sym->ival, IVAL(ISZ_64, IVAL_IMM, .uint_val = stack_size)));
		}
		icode_gen_stmt(cx, expr->stmt);
		emit(cx, ICODE0(IR_RET));
		FUNC_INSTR(enter).arg1.uint_val = cx->code_seg[new_func].top;

		cx->curr_func = old_func;
		return IVAL(type_bytes(expr->type), IVAL_CSO, .uint_val = new_func);

	case EXPR_INTEGER:
		return IVAL(type_bytes(expr->type), IVAL_IMM, .uint_val = expr->uint_val);

	case EXPR_FLOAT:
		return IVAL(type_bytes(expr->type), IVAL_IMM, .float_val = expr->float_val);

	case EXPR_SYM:
		return expr->sym->ival;

	case EXPR_STRING: {
		char* data = lt_arena_reserve(cx->arena, 0);
		usz len = unescape_str(data, LSTR(expr->str_val.str + 1, expr->str_val.len - 2));
		lt_arena_reserve(cx->arena, len);

		usz offs = new_data_seg(cx, SEG_ENT(expr->str_val, len, data));
		return IVAL(type_bytes(expr->type), IVAL_DSO, .uint_val = offs);
	}

	case EXPR_CONVERT: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		if (a1.stype == IVAL_IMM || type_bytes(expr->type) <= a1.size) {
			a1.size = type_bytes(expr->type);
			return a1;
		}

		ival_t dst = IVAL(type_bytes(expr->type), IVAL_REG, .reg = reg__++);
		if (is_int(expr->type) && is_int(expr->child_1->type))
			emit(cx, ICODE(IR_IEXT, dst, a1, IVAL(0, 0)));
		else
			emit(cx, ICODE(IR_UEXT, dst, a1, IVAL(0, 0)));
		return dst;
	}

	case EXPR_DEREFERENCE: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		if (a1.stype & IVAL_REF) {
			ival_t dst = IVAL(type_bytes(expr->type), IVAL_REG, .reg = reg__++);
			emit(cx, ICODE(IR_MOV, dst, a1, IVAL(0, 0)));
			a1 = dst;
		}
		a1.stype |= IVAL_REF;
		a1.size = type_bytes(expr->type);
		return a1;
	}

	case EXPR_REFERENCE: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		LT_ASSERT(a1.stype & IVAL_REF);
		a1.stype &= ~IVAL_REF;
		a1.size = ISZ_64;
		return a1;
	}

	case EXPR_MEMBER: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		LT_ASSERT(expr->child_1->type->stype == TP_STRUCT);
		LT_ASSERT(a1.stype & IVAL_REF);

		LT_ASSERT(expr->child_1->type->child_count > expr->member_index);

		type_t** members = expr->child_1->type->children;
		usz member_offs = 0;
		for (usz i = 0; i < expr->member_index; ++i)
			member_offs += type_bytes(members[i]);

		type_t* member = members[expr->member_index];

		ival_t dst = gen_offset_ref(cx, a1, IVAL(ISZ_64, IVAL_IMM, .uint_val = member_offs));
		dst.size = type_bytes(member);
		return dst;
	}

	case EXPR_CALL: {
		usz arg_i = 0;
		expr_t* it = expr->child_2;
		usz stack_size = 0;
		while (it) {
			emit(cx, ICODE2(IR_SETARG, IVAL(ISZ_64, IVAL_IMM, .uint_val = arg_i++), icode_gen_expr(cx, it)));
			stack_size += type_bytes(it->type);
			it = it->next;
		}
		emit(cx, ICODE2(IR_CALL, icode_gen_expr(cx, expr->child_1), IVAL(ISZ_64, IVAL_IMM, .uint_val = stack_size)));

		if (expr->type->stype == TP_VOID)
			return IVAL(0, 0);

		ival_t dst = IVAL(type_bytes(expr->type), IVAL_REG, .reg = reg__++);
		emit(cx, ICODE1(IR_RETVAL, dst));
		return dst;
	}

	case EXPR_LOGIC_AND: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0)));

		usz jmp1 = emit(cx, ICODE2(IR_CJMPZ, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func), a1));
		ival_t a2 = icode_gen_expr(cx, expr->child_2);
		emit(cx, ICODE2(IR_CSETNZ, dst, a2));

		seg_ent_t* cs = &cx->code_seg[cx->curr_func];
		icode_t* ic = cs->data;
		ic[jmp1].arg2.index = cs->size;
		ic[jmp1].arg2.scale = sizeof(icode_t);
		return dst;
	}
	case EXPR_LOGIC_OR: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1)));

		usz jmp1 = emit(cx, ICODE2(IR_CJMPNZ, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func), a1));
		ival_t a2 = icode_gen_expr(cx, expr->child_2);
		emit(cx, ICODE2(IR_CSETZ, dst, a2)); // FIXME

		seg_ent_t* cs = &cx->code_seg[cx->curr_func];
		icode_t* ic = cs->data;
		ic[jmp1].arg2.index = cs->size;
		ic[jmp1].arg2.scale = sizeof(icode_t);
		return dst;
	}

	case EXPR_LOGIC_NOT: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		if (a1.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = !a1.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0)));
		emit(cx, ICODE2(IR_CSETZ, dst, a1));
		return dst;
	}
	case EXPR_LESSER: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val < a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0)));
		emit(cx, ICODE3(IR_CSETL, dst, a1, a2));
		return dst;
	}
	case EXPR_GREATER: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val > a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0)));
		emit(cx, ICODE3(IR_CSETG, dst, a1, a2));
		return dst;
	}
	case EXPR_LESSER_OR_EQUAL: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val <= a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0)));
		emit(cx, ICODE3(IR_CSETLE, dst, a1, a2));
		return dst;
	}
	case EXPR_GREATER_OR_EQUAL: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val >= a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0)));
		emit(cx, ICODE3(IR_CSETGE, dst, a1, a2));
		return dst;
	}
	case EXPR_EQUAL: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val == a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0)));
		emit(cx, ICODE3(IR_CSETE, dst, a1, a2));
		return dst;
	}
	case EXPR_NOT_EQUAL: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val != a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0)));
		emit(cx, ICODE3(IR_CSETNE, dst, a1, a2));
		return dst;
	}

	case EXPR_SUBSCRIPT: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		ival_t dst = IVAL(ISZ_64, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE2(IR_MOV, dst, a2));
		emit(cx, ICODE3(IR_IMUL, dst, dst, IVAL(ISZ_64, IVAL_IMM, .uint_val = type_bytes(expr->type))));
		emit(cx, ICODE3(IR_ADD, dst, dst, a1));
		dst.stype |= IVAL_REF;
		return dst;
	}

	default:
		break;
	}

	LT_ASSERT_NOT_REACHED();
	return IVAL(0, 0);
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

	case STMT_DEF:
		break;

	case STMT_IF: {
		ival_t cond = icode_gen_expr(cx, stmt->expr);
		usz jmp1 = emit(cx, ICODE2(IR_CJMPZ, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func), cond));
		icode_gen_stmt(cx, stmt->child);

		usz jmp2;
		if (stmt->child_2)
			jmp2 = emit(cx, ICODE1(IR_JMP, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func)));

		FUNC_INSTR(jmp1).arg1.index = CURR_INSTR();
		FUNC_INSTR(jmp1).arg1.scale = sizeof(icode_t);

		if (stmt->child_2) {
			icode_gen_stmt(cx, stmt->child_2);
			FUNC_INSTR(jmp2).arg1.index = CURR_INSTR();
			FUNC_INSTR(jmp2).arg1.scale = sizeof(icode_t);
		}
	}	break;

	case STMT_WHILE: {
		usz eval = CURR_INSTR();
		ival_t cond = icode_gen_expr(cx, stmt->expr);
		usz jmp1 = emit(cx, ICODE2(IR_CJMPZ, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func), cond));

		icode_gen_stmt(cx, stmt->child);

		emit(cx, ICODE1(IR_JMP, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func, .index = eval, .scale = sizeof(icode_t))));
		FUNC_INSTR(jmp1).arg1.index = CURR_INSTR();
		FUNC_INSTR(jmp1).arg1.scale = sizeof(icode_t);
	}	break;

	case STMT_COMPOUND: {
		stmt_t* it = stmt->child;
		while (it) {
			icode_gen_stmt(cx, it);
			it = it->next;
		}
	}	break;

	case STMT_SYSCALL: {
		expr_t* it = stmt->expr;
		usz arg_i = 0;
		while (it) {
			emit(cx, ICODE2(IR_SETARG, IVAL(ISZ_64, IVAL_IMM, .uint_val = arg_i++), icode_gen_expr(cx, it)));
			it = it->next;
		}
		emit(cx, ICODE2(IR_SYSCALL, IVAL(ISZ_64, IVAL_REG, .reg = reg__++), IVAL(ISZ_64, IVAL_IMM, .uint_val = arg_i)));
	}	break;

	case STMT_RETURN: {
		ival_t val = IVAL(0, 0);
		if (stmt->expr)
			val = icode_gen_expr(cx, stmt->expr);
		emit(cx, ICODE(IR_RET, val, IVAL(0, 0), IVAL(0, 0)));
		break;
	}

	default:
		LT_ASSERT_NOT_REACHED();
	}
}

void icode_gen(gen_ctx_t* cx, stmt_t* root) {
	stmt_t* it = root;
	while (it) {
		icode_gen_stmt(cx, it);
		it = it->next;
	}
}


