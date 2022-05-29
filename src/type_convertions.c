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

	if ((is_int(type) || is_uint(type) || is_float(type) || is_bool(type) ||
		type->stype == TP_PTR || type->stype == TP_FUNC)
		&&
		(is_int(old->type) || is_uint(old->type) || is_float(old->type) || is_bool(old->type) ||
		old->type->stype == TP_PTR || old->type->stype == TP_FUNC))
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

void type_make_compatible(parse_ctx_t* cx, tk_t* tk, int stype, expr_t** left, expr_t** right) {
	LT_ASSERT(left && *left && (*left)->type);
	LT_ASSERT(right && *right && (*right)->type);

	type_t* from = NULL;
	type_t* to = NULL;

	// TODO: promote types to higher precision when possible

	if ((*left)->type->stype == TP_VOID)
		ferr("void type in expression", *tk);
	if ((*right)->type->stype == TP_VOID)
		ferr("void type in expression", *tk);

	switch (stype) {
	case EXPR_LOGIC_AND: case EXPR_LOGIC_OR:
		if (!is_number((*left)->type) || !is_number((*right)->type))
			ferr("operands of binary "A_BOLD"'%S'"A_RESET" must be valid numbers", *tk, tk->str);
		return;

	case EXPR_BIT_SHIFT_LEFT: case EXPR_BIT_SHIFT_RIGHT:
		from = (*right)->type;
		to = &u8_def;
		if (!type_convert_implicit(cx, to, right))
			goto implicit_err;
		return;

	case EXPR_LESSER: case EXPR_GREATER:
	case EXPR_LESSER_OR_EQUAL: case EXPR_GREATER_OR_EQUAL:
		from = (*right)->type;
		to = (*left)->type;
		if (is_int_any_sign((*left)->type) && is_int_any_sign((*right)->type)) {
			b8 l_signed = is_int((*left)->type);
			b8 r_signed = is_int((*right)->type);

			b8 differing_sign = l_signed ^ r_signed;
			if (differing_sign)
				goto implicit_err;
		}
		if (!type_convert_implicit(cx, to, right))
			goto implicit_err;
		return;

	case EXPR_EQUAL: case EXPR_NOT_EQUAL: // TODO
	case EXPR_ADD:
	case EXPR_SUBTRACT:
	case EXPR_MULTIPLY:
	case EXPR_DIVIDE:
	case EXPR_MODULO:
		if (!is_number((*left)->type) || !is_number((*right)->type))
			ferr("operands of binary "A_BOLD"'%S'"A_RESET" must be valid numbers", *tk, tk->str);

		to = (*left)->type;
		from = (*right)->type;
		if (type_eq(to, from))
			return;
		if (!type_convert_implicit(cx, to, right))
			goto implicit_err;
		return;


	case EXPR_ASSIGN:
		from = (*right)->type;
		to = (*left)->type;

		if (!type_convert_implicit(cx, to, right))
			goto implicit_err;
		return;

	case EXPR_BIT_AND:
	case EXPR_BIT_OR:
	case EXPR_BIT_XOR:
		if (!is_int_any_sign((*left)->type) || !is_int_any_sign((*right)->type))
			ferr("operands of binary "A_BOLD"'%S'"A_RESET" must have integer operands", *tk, tk->str);
		if (!type_eq((*left)->type, (*right)->type))
			ferr("bitwise operator must have operands of same type", *tk);
		return;

	case EXPR_VIEW:
		to = &i64_def;

		from = (*left)->type;
		if (!type_convert_implicit(cx, to, left))
			goto implicit_err;
		from = (*right)->type;
		if (!type_convert_implicit(cx, to, right))
			goto implicit_err;
		return;

	default:
		return;
	}

implicit_err:
	ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
			type_to_reserved_str(cx->arena, from), type_to_reserved_str(cx->arena, to));
}

