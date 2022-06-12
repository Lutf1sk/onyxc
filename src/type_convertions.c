#include "type.h"
#include "expr_ast.h"
#include "parse.h"
#include "textattrib.h"
#include "err.h"

b8 type_convert_implicit(parse_ctx_t* cx, type_t* type, expr_t** expr) {
	expr_t* old = *expr;

	if (type_eq(type, old->type))
		return 1;

	b8 void_arr1 = type_eq(old->type, &void_view_def) && type->stype == TP_ARRAY;
	b8 void_arr2 = old->type->stype == TP_ARRAY && type_eq(type, &void_view_def);
	if (void_arr1 || void_arr2) {
		type_t* base = type->base;
		if (void_arr2)
			base = old->type->base;
		type_t* view_type = lt_arena_reserve(cx->arena, sizeof(type_t));
		*view_type = TYPE(TP_ARRAY_VIEW, base);

		expr_t* view = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*view = EXPR(EXPR_VIEW, view_type, (*expr)->tk);
		view->child_1 = (*expr);

		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_CONVERT, type, (*expr)->tk);
		new->child_1 = view;

		*expr = new;
		return 1;
	}

	if ((is_int_any_sign(type) && is_int_any_sign(old->type)) ||
		(is_float(type) && is_float(old->type)) ||
		(is_bool(type) && is_bool(old->type)) ||
		(type_eq(old->type, &void_ptr_def) && (type->stype == TP_PTR || type->stype == TP_FUNC)) ||
		((old->type->stype == TP_PTR || old->type->stype == TP_FUNC) && type_eq(type, &void_ptr_def)) ||

		(type_eq(old->type, &void_view_def) && type->stype == TP_ARRAY_VIEW) ||
		(old->type->stype == TP_ARRAY_VIEW && type_eq(type, &void_view_def)))
	{
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_CONVERT, type, (*expr)->tk);
		new->child_1 = old;
		*expr = new;
		return 1;
	}

	if (type->stype == TP_ARRAY_VIEW && old->type->stype == TP_ARRAY && type_eq(type->base, old->type->base)) {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_VIEW, type, (*expr)->tk);
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

	if ((is_int_any_sign_or_ptr(old->type) || is_float(type) || is_bool(type)) &&
		(is_int_any_sign_or_ptr(old->type) || is_float(old->type) || is_bool(old->type)))
	{
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_CONVERT, type, (*expr)->tk);
		new->child_1 = old;
		*expr = new;
		return 1;
	}

	if (type->stype == TP_ARRAY_VIEW && old->type->stype == TP_ARRAY) {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_VIEW, type, (*expr)->tk);
		new->child_1 = old;
		*expr = new;
		return 1;
	}

	return 0;
}

type_t* type_make_compatible(parse_ctx_t* cx, tk_t* tk, int stype, expr_t** left, expr_t** right) {
	LT_ASSERT(left && *left && (*left)->type);
	LT_ASSERT(right && *right && (*right)->type);

	if ((*left)->type->stype == TP_VOID)
		ferr("void type in expression", *(*left)->tk);
	if ((*right)->type->stype == TP_VOID)
		ferr("void type in expression", *(*right)->tk);

	expr_t** from = NULL;
	expr_t** to = NULL;

	expr_t** small = right;
	expr_t** large = left;
	if ((type_bytes((*small)->type) > type_bytes((*large)->type)) ||
		((*small)->type->stype == TP_PTR && is_int_any_sign((*large)->type)) ||
		((*small)->type->stype == TP_PTR && type_eq((*large)->type, &void_ptr_def)))
	{
		small = left;
		large = right;
	}

	switch (stype) {
	case EXPR_LOGIC_AND: case EXPR_LOGIC_OR:
		if (!is_number((*left)->type) || !is_number((*right)->type))
			ferr("operands of binary "A_BOLD"'%S'"A_RESET" must be valid numbers", *tk, tk->str);
		return &u8_def;

	case EXPR_EQUAL: case EXPR_NOT_EQUAL:
	case EXPR_LESSER: case EXPR_GREATER:
	case EXPR_LESSER_OR_EQUAL: case EXPR_GREATER_OR_EQUAL:
		from = small;
		to = large;
		if (is_int_any_sign((*left)->type) && is_int_any_sign((*right)->type)) {
			b8 l_signed = is_int((*left)->type);
			b8 r_signed = is_int((*right)->type);

			b8 differing_sign = l_signed ^ r_signed;
			if (differing_sign)
				werr("comparison of integers of differing sign", *tk);
		}
		if (!type_convert_implicit(cx, (*to)->type, from))
			goto implicit_err;
		return &u8_def;


	case EXPR_BIT_AND:
	case EXPR_BIT_OR:
	case EXPR_BIT_XOR:
	case EXPR_BIT_SHIFT_LEFT:
	case EXPR_BIT_SHIFT_RIGHT:
		if (!is_int_any_sign_or_ptr((*left)->type) || !is_int_any_sign_or_ptr((*right)->type))
			ferr("operands of binary "A_BOLD"'%S'"A_RESET" must have integer operands", *tk, tk->str);
	case EXPR_ADD:
	case EXPR_SUBTRACT:
	case EXPR_MULTIPLY:
	case EXPR_DIVIDE:
	case EXPR_MODULO:
		if (!is_number((*left)->type) || !is_number((*right)->type))
			ferr("operands of binary "A_BOLD"'%S'"A_RESET" must be valid numbers", *tk, tk->str);

		from = small;
		to = large;

		if ((*to)->type->stype == TP_PTR && (*from)->type->stype != TP_PTR) {
			if (!type_convert_explicit(cx, (*to)->type, from))
				goto implicit_err;
		}
		else {
			if (!type_convert_implicit(cx, (*to)->type, from))
				goto implicit_err;
		}
		return (*to)->type;

	case EXPR_BIT_SHIFT_LEFT_ASSIGN:
	case EXPR_BIT_SHIFT_RIGHT_ASSIGN:
	case EXPR_BIT_AND_ASSIGN:
	case EXPR_BIT_XOR_ASSIGN:
	case EXPR_BIT_OR_ASSIGN:
		if (!is_int_any_sign_or_ptr((*left)->type) || !is_int_any_sign_or_ptr((*right)->type))
			ferr("operands of "A_BOLD"'%S'"A_RESET" must have integer operands", *tk, tk->str);
	case EXPR_ADD_ASSIGN:
	case EXPR_SUBTRACT_ASSIGN:
	case EXPR_MULTIPLY_ASSIGN:
	case EXPR_DIVIDE_ASSIGN:
	case EXPR_MODULO_ASSIGN:
		if (!is_number((*left)->type) || !is_number((*right)->type))
			ferr("operands of "A_BOLD"'%S'"A_RESET" must be valid numbers", *tk, tk->str);
	case EXPR_ASSIGN:
		from = right;
		to = left;

		if ((*to)->type->stype == TP_PTR && (*from)->type->stype != TP_PTR) {
			if (!type_convert_explicit(cx, (*to)->type, from))
				goto implicit_err;
		}
		else {
			if (!type_convert_implicit(cx, (*to)->type, from))
				goto implicit_err;
		}
		return (*to)->type;

	case EXPR_UFCS:
		return (*right)->type->base;

	default:
		lt_ferrf("Unhandled expression type %S\n", expr_type_str(stype));
	}

implicit_err:
	ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
			type_to_reserved_str(cx->arena, (*from)->type), type_to_reserved_str(cx->arena, (*to)->type));
}

