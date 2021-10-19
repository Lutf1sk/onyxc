#include "parse.h"
#include "parse_helpers.h"
#include "expr_ast.h"
#include "stmt_ast.h"
#include "type.h"

#include "symtab.h"

#include <lt/str.h>

expr_t* parse_expr_primary(parse_ctx_t* cx, type_t* type) {
	tk_t tk = *peek(cx, 0);

	if (type && type->stype == TP_FUNC && tk.stype == TK_LEFT_BRACE) {
		type_t* old_func_type = cx->curr_func_type;
		cx->curr_func_type = type;

		stmt_t* compound = parse_func_body(cx);
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_LAMBDA, type);
		new->stmt = compound;

		cx->curr_func_type = old_func_type;
		return new;
	}

	switch (tk.stype) {
	case TK_LEFT_PARENTH: consume(cx); {
		expr_t* new = parse_expr(cx, type);
		consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"')'"A_RESET));
		return new;
	}

	case TK_INT: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &i64_def);
		new->int_val = lt_lstr_int(tk.str);
		return new;
	}

	case TK_UINT: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &u64_def);
		new->uint_val = lt_lstr_uint(tk.str);;
		return new;
	}

	case TK_FLOAT: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_FLOAT, &f64_def);
		new->float_val = lt_lstr_float(tk.str);;
		return new;
	}

	case TK_CHAR: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &u8_def);

		char* str = lt_arena_reserve(cx->arena, 0);
		usz len = unescape_str(str, LSTR(tk.str.str + 1, tk.str.len - 2));

		if (len > 1)
			ferr("character literal exceeds max length", cx->lex, tk);
		else if (!len)
			ferr("empty character literal", cx->lex, tk);

		new->int_val = str[0];
		return new;
	}

	case TK_STRING: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_STRING, &u8_ptr_def);
		new->str_val = tk.str;
		return new;
	}

	case TK_KW_NULL: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &void_ptr_def);
		new->uint_val = 0;
		return new;
	}

	case TK_IDENTIFIER: {
		sym_t* sym = symtab_find(cx->symtab, tk.str);
		if (!sym)
			goto undeclared;

		if (sym->stype == SYM_VAR) {
			consume(cx);
			expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*new = EXPR(EXPR_SYM, sym->type);
			sym->flags |= SYMFL_ACCESSED;
			new->sym = sym;
			return new;
		}
		if (sym->stype == SYM_TYPE) {
			type_t* init_type = parse_type(cx);

			if (type && !type_eq(type, init_type))
				ferr("unexpected type "A_BOLD"'%S'"A_RESET" in initializer of "A_BOLD"'%S'"A_RESET, cx->lex, tk,
						type_to_reserved_str(cx->arena, init_type), type_to_reserved_str(cx->arena, type));

			tk_t* tk = peek(cx, 0);
			if (tk->stype == TK_COLON) {
				consume(cx);
				expr_t* expr = parse_expr_unary(cx, NULL);
				if (!type_convert_explicit(cx, init_type, &expr))
					ferr("cannot convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, cx->lex, *tk,
							type_to_reserved_str(cx->arena, expr->type), type_to_reserved_str(cx->arena, init_type));
				return expr;
			}

			return parse_expr_unary(cx, init_type);
		}

	undeclared:
		ferr("use of undeclared identifier "A_BOLD"'%S'"A_RESET, cx->lex, tk, tk.str);
	}

	default:
		ferr("unexpected token "A_BOLD"'%S'"A_RESET" in expression", cx->lex, tk, tk.str);
	}
}

static
operator_t* find_sfx_operator(tk_stype_t tk_type) {
	if (sfx_operators[tk_type].tk != 0xFFFFFFFF)
		return &sfx_operators[tk_type];
	return NULL;
}

static
operator_t* find_pfx_operator(tk_stype_t tk_type) {
	if (pfx_operators[tk_type].tk != 0xFFFFFFFF)
		return &pfx_operators[tk_type];
	return NULL;
}

static
expr_t* parse_expr_unary_sfx(parse_ctx_t* cx, type_t* type) {
	expr_t* current = parse_expr_primary(cx, type);

	operator_t* op;
	while ((op = find_sfx_operator(peek(cx, 0)->stype))) {
		tk_t* tk = consume(cx);

		if (op->expr == EXPR_MEMBER) {
			tk_t* member_name_tk = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected member name"));
			type_t* it = current->type;
			while (it->stype == TP_PTR) {
				expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
				*new = EXPR(EXPR_DEREFERENCE, it->base);
				new->child_1 = current;
				current = new;

				it = it->base;
			}

			if (it->stype != TP_STRUCT)
				ferr("cannot use "A_BOLD"'.'"A_RESET" operator on a non-structure type", cx->lex, *tk);

			lstr_t member_name = member_name_tk->str;
			usz member_count = it->child_count;
			usz member_index;
			for (usz i = 0; i < member_count; ++i) {
				if (lt_lstr_eq(it->child_names[i], member_name)) {
					member_index = i;
					goto member_found;
				}
			}

			ferr("structure has no member named "A_BOLD"'%S'"A_RESET, cx->lex, *member_name_tk, member_name_tk->str);

		member_found:

			expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*new = EXPR(EXPR_MEMBER, it->children[member_index]);
			new->child_1 = current;
			new->member_index = member_index;
			current = new;
			continue;
		}

		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(op->expr, current->type);
		new->child_1 = current;
		current = new;

		if (op->expr == EXPR_SUBSCRIPT) {
			type_t* new_type = new->type;
			if (new_type->stype != TP_PTR && new_type->stype != TP_ARRAY && new_type->stype != TP_ARRAY_VIEW)
				ferr("subscripted type "A_BOLD"'%S'"A_RESET" is neither an array nor a pointer",
						cx->lex, *tk, type_to_reserved_str(cx->arena, new->type));

			new->child_2 = parse_expr(cx, NULL);
			if (!is_int_any_sign(new->child_2->type))
				ferr("array index must be an integer", cx->lex, *tk);
			consume_type(cx, TK_RIGHT_BRACKET, CLSTR(", expected "A_BOLD"']'"A_RESET" after array subscript"));

			new->type = new_type->base;
		}
		else if (op->expr == EXPR_CALL) {
			if (new->type->stype != TP_FUNC)
				ferr("called type "A_BOLD"'%S'"A_RESET" is not a function", cx->lex, *tk,
						type_to_reserved_str(cx->arena, new->type));

			expr_t** current_arg = &new->child_2;

			type_t** arg_types = new->child_1->type->children;
			usz arg_i = 0, arg_count = new->child_1->type->child_count;

			lstr_t func_name = CLSTR("function");
			if (new->child_1->stype == EXPR_SYM)
				func_name = new->child_1->sym->name;

			while (peek(cx, 0)->stype != TK_RIGHT_PARENTH) {
				if (current_arg != &new->child_2)
					consume_type(cx, TK_COMMA, CLSTR(", expected "A_BOLD"','"A_RESET" or "A_BOLD"')'"A_RESET));

				expr_t* arg = parse_expr(cx, NULL);
				if (arg_i == arg_count)
					ferr("too many arguments to "A_BOLD"'%S'"A_RESET", expected %uq", cx->lex, *tk, func_name, arg_count);
				if (!type_convert_implicit(cx, arg_types[arg_i], &arg))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, cx->lex, *tk,
							type_to_reserved_str(cx->arena, arg->type),
							type_to_reserved_str(cx->arena, arg_types[arg_i]));
				*current_arg = arg;
				current_arg = &arg->next;
				++arg_i;
			}
			if (arg_i != arg_count)
				ferr("too few arguments to "A_BOLD"'%S'"A_RESET", expected %uq", cx->lex, *tk, func_name, arg_count);
			consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"')'"A_RESET" after function call"));

			new->type = new->type->base;
		}
	}

	return current;
}

expr_t* parse_expr_unary(parse_ctx_t* cx, type_t* type) {
	tk_t tk = *peek(cx, 0);

	operator_t* op = find_pfx_operator(tk.stype);
	if (op) {
		consume(cx);
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(op->expr, NULL);

		expr_t* child = parse_expr_unary(cx, type);
		new->child_1 = child;

		switch (op->expr) {
		case EXPR_DEREFERENCE:
			if (child->type->stype != TP_PTR)
				ferr("dereferenced type "A_BOLD"'%S'"A_RESET" is not a pointer", cx->lex, tk,
						type_to_reserved_str(cx->arena, child->type));
			new->type = child->type->base;
			break;

		case EXPR_REFERENCE: {
			type_t* type = lt_arena_reserve(cx->arena, sizeof(type_t));
			*type = TYPE(TP_PTR, child->type);
			if (child->stype == EXPR_SYM)
				child->sym->flags |= SYMFL_REFERENCED;
			new->type = type;
		}	break;

		default:
			new->type = child->type;
			break;
		}
		return new;
	}
	return parse_expr_unary_sfx(cx, type);
}

static
operator_t* find_binary_operator(tk_stype_t tk_type) {
	if (operators[tk_type].tk != 0xFFFFFFFF)
		return &operators[tk_type];
	return NULL;
}

expr_t* parse_expr_binary(parse_ctx_t* cx, type_t* type, int precedence) {
	expr_t* left = parse_expr_unary(cx, type);

	tk_t* tk = peek(cx, 0);

	operator_t* op = find_binary_operator(tk->stype);
	while (op && (op->precedence < precedence || (op->precedence == precedence && op->associate == OP_ASSOC_RIGHT))) {
		consume(cx);
		expr_t* right = parse_expr_binary(cx, NULL, op->precedence);
		type_t* type = left->type;

		type_make_compatible(cx, tk->line_index, op->expr, &left, &right);

		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(op->expr, type);
		new->child_1 = left;
		new->child_2 = right;

		left = new;

		tk = peek(cx, 0);
		op = find_binary_operator(tk->stype);
	}

	return left;
}

expr_t* parse_expr(parse_ctx_t* cx, type_t* type) {
	return parse_expr_binary(cx, type, 100);
}

