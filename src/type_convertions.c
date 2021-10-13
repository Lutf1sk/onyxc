#include "type.h"
#include "expr_ast.h"
#include "parse.h"

b8 type_convert_implicit(parse_ctx_t* cx, type_t* type, expr_t** expr) {
	expr_t* old = *expr;

	if (type_eq(type, old->type))
		return 1;

	if ((is_int(type) && is_int(old->type)) ||
		(is_uint(type) && is_uint(old->type)) ||
		(is_float(type) && is_float(old->type)) ||
		(is_bool(type) && is_bool(old->type)) ||
		(old->type->stype == TP_PTR && old->type->base->stype == TP_VOID && type->stype == TP_PTR) ||
		(type->stype == TP_PTR && type->base->stype == TP_VOID && old->type->stype == TP_PTR))
	{
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_CONVERT, type);
		new->child_1 = old;
		*expr = new;
		return 1;
	}

	return 0;
}

b8 type_convert_explicit(parse_ctx_t* cx, type_t* type, expr_t** expr) {
	if (type_convert_implicit(cx, type, expr))
		return 1;

	expr_t* old = *expr;

	if ((is_int(type) || is_uint(type) || is_float(type) || is_bool(type) ||
		type->stype == TP_PTR || type->stype == TP_FUNC)
		&&
		(is_int(old->type) || is_uint(old->type) || is_float(old->type) || is_bool(old->type) ||
		old->type->stype == TP_PTR || old->type->stype == TP_FUNC))
	{
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_CONVERT, type);
		new->child_1 = old;
		*expr = new;
		return 1;
	}

	return 0;
}

void type_make_compatible(parse_ctx_t* cx, usz line_index, int stype, expr_t** left, expr_t** right) {
	type_t* from = NULL;
	type_t* to = NULL;

	switch (stype) {
	case EXPR_LOGIC_AND: case EXPR_LOGIC_OR:
		if (!is_scalar(from) || !is_scalar(to))
			lt_ferrf("%s:%uz: Operands to logical operator must be scalar\n");
		return;

	case EXPR_BIT_SHIFT_LEFT: case EXPR_BIT_SHIFT_RIGHT:
		from = (*right)->type;
		to = &u8_def;
		if (!type_convert_implicit(cx, to, right))
			goto implicit_err;
		return;

	case EXPR_LESSER: case EXPR_GREATER:
	case EXPR_LESSER_OR_EQUAL: case EXPR_GREATER_OR_EQUAL:
	case EXPR_EQUAL: case EXPR_NOT_EQUAL:
	case EXPR_ADD:
	case EXPR_SUBTRACT:
	case EXPR_MULTIPLY:
	case EXPR_DIVIDE:
	case EXPR_MODULO:
		if (type_eq((*left)->type, (*right)->type))
			return;

		if (is_int_any_sign((*left)->type) && is_int_any_sign((*right)->type)) {
			b8 l_signed = is_int((*left)->type);
			b8 r_signed = is_int((*right)->type);

			b8 differing_sign = l_signed ^ r_signed;
			if (differing_sign) {
				from = (*right)->type = (*right)->type;
				to = (*left)->type;
				goto implicit_err;
			}
		}

	case EXPR_ASSIGN:
		from = (*right)->type = (*right)->type;
		to = (*left)->type;

		if (!type_convert_implicit(cx, (*left)->type, right))
			goto implicit_err;
		return;

	case EXPR_BIT_AND:
	case EXPR_BIT_OR:
	case EXPR_BIT_XOR:
		if (!type_eq((*left)->type, (*right)->type))
			lt_ferrf("%s:%uz: Bitwise operator must have operands of same type\n", cx->path, line_index + 1);
		return;
	}

	return;

implicit_err:
	lt_ferrf("%s:%uz: Cannot implicitly convert %S to %S\n", cx->path, line_index + 1,
			type_to_reserved_str(cx->arena, from), type_to_reserved_str(cx->arena, to));
}

