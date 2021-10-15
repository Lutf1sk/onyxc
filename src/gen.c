#include <lt/mem.h>

#include "interm.h"
#include "gen.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "type.h"
#include "symtab.h"
#include "segment.h"

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
usz new_code_seg(gen_ctx_t* cx) {
	if (!(cx->code_seg_count & SEGMENT_BLOCK_MASK))
		cx->code_seg = realloc(cx->code_seg, (cx->code_seg_count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	memset(&cx->code_seg[cx->code_seg_count], 0, sizeof(seg_ent_t));
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
			usz size = type_bytes(sym->type);
			ival_t val = IVAL(ISZ_64, IVAL_IMM, .uint_val = size);
			u8 stack_op = IR_SRESV;
			u8 flags = sym->flags;
			b8 scalar = is_scalar(sym->type);

			if (expr && scalar) {
				val = icode_gen_expr(cx, expr);
				stack_op = IR_PUSH;
			}

			if (!(flags & SYMFL_ACCESSED))
				return;

			usz sf_offs = cx->code_seg[cx->curr_func].top;
			cx->code_seg[cx->curr_func].top += size;

			if (!scalar) {
				emit(cx, ICODE(IR_SRESV, IVAL(ISZ_64, IVAL_IMM, .uint_val = size), IVAL(0, 0), IVAL(0, 0)));
				val = IVAL(size, IVAL_SFO | IVAL_REF, .uint_val = sf_offs);
				if (expr) {
					ival_t init = icode_gen_expr(cx, expr);
					emit(cx, ICODE(IR_COPY, val, init, IVAL(0, 0)));
				}
			}
			else if (flags & SYMFL_REFERENCED) {
				emit(cx, ICODE(stack_op, val, IVAL(0, 0), IVAL(0, 0)));
				val = IVAL(size, IVAL_SFO | IVAL_REF, .uint_val = sf_offs);
			}
			else {
				ival_t dst = IVAL(size, IVAL_REG, .reg = reg__++);
				if (expr)
					emit(cx, ICODE(IR_MOV, dst, val, IVAL(0, 0)));
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

	case EXPR_PFX_INCREMENT:
		GENERIC_EXPR_UNARY(IR_INC, ++);

	case EXPR_PFX_DECREMENT:
		GENERIC_EXPR_UNARY(IR_DEC, --);

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
		isz old_func = cx->curr_func, new_func = new_code_seg(cx);
		cx->curr_func = new_func;

		sym_t** args = expr->type->child_syms;
		usz arg_count = expr->type->child_count;
		for (usz i = 0; i < arg_count; ++i) {
			sym_t* sym = args[i];
			if (!sym || !(sym->flags & SYMFL_ACCESSED))
				continue;
			gen_sym_def(cx, sym, NULL);
			emit(cx, ICODE(IR_GETARG, sym->ival, IVAL(ISZ_64, IVAL_IMM, .uint_val = i), IVAL(0, 0)));
		}

		icode_gen_stmt(cx, expr->stmt);
		emit(cx, ICODE(IR_RET, IVAL(0, 0), IVAL(0, 0), IVAL(0, 0)));

		cx->curr_func = old_func;
		return IVAL(type_bytes(expr->type), IVAL_CSO, .uint_val = new_func);

	case EXPR_INTEGER:
		return IVAL(type_bytes(expr->type), IVAL_IMM, .uint_val = expr->uint_val);

	case EXPR_FLOAT:
		return IVAL(type_bytes(expr->type), IVAL_IMM, .float_val = expr->float_val);

	case EXPR_SYM:
		return expr->sym->ival;

	case EXPR_STRING: {
		usz offs = new_data_seg(cx, SEG_ENT(expr->str_val, expr->str_val.len, expr->str_val.str));
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
		while (it) {
			emit(cx, ICODE(IR_SETARG, icode_gen_expr(cx, it), IVAL(ISZ_64, IVAL_IMM, .uint_val = arg_i++), IVAL(0, 0)));
			it = it->next;
		}
		emit(cx, ICODE(IR_CALL, icode_gen_expr(cx, expr->child_1), IVAL(0, 0), IVAL(0, 0)));

		if (expr->type->stype == TP_VOID)
			return IVAL(0, 0);

		ival_t dst = IVAL(type_bytes(expr->type), IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_RETVAL, dst, IVAL(0, 0), IVAL(0, 0)));
		return dst;
	}

	case EXPR_LOGIC_AND: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), IVAL(0, 0)));

		usz jmp1 = emit(cx, ICODE(IR_CJMPZ, a1, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func, .instr = 0), IVAL(0, 0)));
		ival_t a2 = icode_gen_expr(cx, expr->child_2);
		emit(cx, ICODE(IR_CMOVNZ, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), a2));

		seg_ent_t* cs = &cx->code_seg[cx->curr_func];
		icode_t* ic = cs->data;
		ic[jmp1].arg2.instr = cs->size;
		return dst;
	}
	case EXPR_LOGIC_OR: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), IVAL(0, 0)));

		usz jmp1 = emit(cx, ICODE(IR_CJMPNZ, a1, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func, .instr = 0), IVAL(0, 0)));
		ival_t a2 = icode_gen_expr(cx, expr->child_2);
		emit(cx, ICODE(IR_CMOVZ, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), a2));

		seg_ent_t* cs = &cx->code_seg[cx->curr_func];
		icode_t* ic = cs->data;
		ic[jmp1].arg2.instr = cs->size;
		return dst;
	}

	case EXPR_LOGIC_NOT: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1);
		if (a1.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = !a1.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), IVAL(0, 0)));
		emit(cx, ICODE(IR_CMOVZ, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), a1));
		return dst;
	}
	case EXPR_LESSER: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val < a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), IVAL(0, 0)));
		emit(cx, ICODE(IR_CMOVL, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), a1));
		return dst;
	}
	case EXPR_GREATER: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val > a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), IVAL(0, 0)));
		emit(cx, ICODE(IR_CMOVG, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), a1));
		return dst;
	}
	case EXPR_LESSER_OR_EQUAL: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val <= a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), IVAL(0, 0)));
		emit(cx, ICODE(IR_CMOVLE, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), a1));
		return dst;
	}
	case EXPR_GREATER_OR_EQUAL: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val >= a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), IVAL(0, 0)));
		emit(cx, ICODE(IR_CMOVGE, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), a1));
		return dst;
	}
	case EXPR_EQUAL: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val == a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), IVAL(0, 0)));
		emit(cx, ICODE(IR_CMOVE, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), a1));
		return dst;
	}
	case EXPR_NOT_EQUAL: {
		ival_t a1 = icode_gen_expr(cx, expr->child_1), a2 = icode_gen_expr(cx, expr->child_2);
		if (a1.stype == IVAL_IMM && a2.stype == IVAL_IMM)
			return IVAL(ISZ_8, IVAL_IMM, .uint_val = a1.uint_val != a2.uint_val);

		ival_t dst = IVAL(ISZ_8, IVAL_REG, .reg = reg__++);
		emit(cx, ICODE(IR_MOV, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 0), IVAL(0, 0)));
		emit(cx, ICODE(IR_CMOVNE, dst, IVAL(ISZ_8, IVAL_IMM, .uint_val = 1), a1));
		return dst;
	}

	default:
		break;
	}

	LT_ASSERT_NOT_REACHED();
	return IVAL(0, 0);
}

#define FUNC_INSTR(x) (((icode_t*)cx->code_seg[cx->curr_func].data)[(x)])
#define CURR_INSTR() (cx->code_seg[cx->curr_func].size)

void icode_gen_stmt(gen_ctx_t* cx, stmt_t* stmt) {
	switch (stmt->stype) {
	case STMT_LET:
		gen_sym_def(cx, stmt->sym, stmt->expr);
		break;

	case STMT_EXPR:
		icode_gen_expr(cx, stmt->expr);
		break;

	case STMT_DEF:
		break;

	case STMT_IF: {
		ival_t cond = icode_gen_expr(cx, stmt->expr);
		usz jmp1 = emit(cx, ICODE(IR_CJMPZ, cond, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func, .instr = 0), IVAL(0, 0)));
		icode_gen_stmt(cx, stmt->child);

		usz jmp2;
		if (stmt->child_2)
			jmp2 = emit(cx, ICODE(IR_JMP, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func, .instr = 0), IVAL(0, 0), IVAL(0, 0)));

		FUNC_INSTR(jmp1).arg2.instr = CURR_INSTR();

		if (stmt->child_2) {
			icode_gen_stmt(cx, stmt->child_2);
			FUNC_INSTR(jmp2).arg1.instr = CURR_INSTR();
		}
	}	break;

	case STMT_WHILE: {
		usz eval = CURR_INSTR();
		ival_t cond = icode_gen_expr(cx, stmt->expr);
		usz jmp1 = emit(cx, ICODE(IR_CJMPZ, cond, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func, .instr = 0), IVAL(0, 0)));

		icode_gen_stmt(cx, stmt->child);

		emit(cx, ICODE(IR_JMP, IVAL(ISZ_64, IVAL_CSO, .cso = cx->curr_func, .instr = eval), IVAL(0, 0), IVAL(0, 0)));
		FUNC_INSTR(jmp1).arg2.instr = CURR_INSTR();
	}	break;

	case STMT_COMPOUND:
		stmt_t* it = stmt->child;
		while (it) {
			icode_gen_stmt(cx, it);
			it = it->next;
		}
		break;

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


