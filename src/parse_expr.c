#include "parse.h"
#include "parse_helpers.h"
#include "expr_ast.h"
#include "stmt_ast.h"
#include "type.h"

#include "tk_ctype.h"

#include "symtab.h"

#include <lt/str.h>

expr_t* parse_expr_primary(parse_ctx_t* cx, type_t* type) {
	tk_t* tk = peek(cx, 0);

	if (type) {
		if (type->stype == TP_FUNC && tk->stype == TK_LEFT_BRACE) {
			type_t* old_func_type = cx->curr_func_type;
			cx->curr_func_type = type;

			symtab_t* lbltab = lt_arena_reserve(cx->arena, sizeof(symtab_t));
			stmt_t* compound = parse_func_body(cx, lbltab);
			expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*new = EXPR(EXPR_LAMBDA, type, tk);
			new->stmt = compound;
			new->label_symtab = lbltab;

			cx->curr_func_type = old_func_type;
			return new;
		}
		else if (type->stype == TP_ARRAY && tk->stype == TK_LEFT_BRACE) {
			consume(cx);
			expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*new = EXPR(EXPR_ARRAY, type, tk);

			expr_t** it = &new->child_1;
			usz count = 0;
			while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
				tk_t* tk = peek(cx, 0);
				expr_t* new = parse_expr(cx, type->base);
				if (!type_convert_implicit(cx, type->base, &new))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
							type_to_reserved_str(cx->arena, new->type), type_to_reserved_str(cx->arena, type->base));
				*it = new;
				it = &new->next;
				++count;

				if (peek(cx, 0)->stype != TK_COMMA)
					break;
				consume(cx);
			}
			consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected "A_BOLD"'}'"A_RESET));
			return new;
		}
		else if (type->stype == TP_ARRAY_VIEW && tk->stype == TK_LEFT_BRACE) {
			consume(cx);
			type_t* new_type = lt_arena_reserve(cx->arena, sizeof(type_t));
			*new_type = TYPE(TP_ARRAY, type->base);

			expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*new = EXPR(EXPR_ARRAY, new_type, tk);

			expr_t** it = &new->child_1;
			usz count = 0;
			while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
				tk_t* tk = peek(cx, 0);
				expr_t* new = parse_expr(cx, new_type->base);
				if (!type_convert_implicit(cx, type->base, &new))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
							type_to_reserved_str(cx->arena, new->type), type_to_reserved_str(cx->arena, type->base));
				*it = new;
				it = &new->next;
				++count;

				if (peek(cx, 0)->stype != TK_COMMA)
					break;
				consume(cx);
			}
			consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected "A_BOLD"'}'"A_RESET));
			new_type->child_count = count;
			return new;
		}
		else if (type->stype == TP_STRUCT && tk->stype == TK_LEFT_BRACE) {
			consume(cx);
			expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*new = EXPR(EXPR_STRUCT, type, tk);

			expr_t** it = &new->child_1;
			usz i = 0;
			while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
				tk_t* tk = peek(cx, 0);

				if (i >= type->child_count)
					ferr("excess elements in struct initializer", *tk);

				expr_t* new = parse_expr(cx, type->children[i]);
				if (!type_convert_implicit(cx, type->children[i], &new))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
							type_to_reserved_str(cx->arena, new->type), type_to_reserved_str(cx->arena, type->children[i]));

				*it = new;
				it = &new->next;
				++i;

				if (peek(cx, 0)->stype != TK_COMMA)
					break;
				consume(cx);
			}
			if (i < type->child_count)
				werr("uninitialized elements of "A_BOLD"'%S'"A_RESET, *tk,
						type_to_reserved_str(cx->arena, type));
			consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected "A_BOLD"'}'"A_RESET));
			return new;
		}
	}

	switch (tk->stype) {
	case TK_LEFT_PARENTH: consume(cx); {
		expr_t* new = parse_expr(cx, type);
		consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"')'"A_RESET));
		return new;
	}

	case TK_NUMBER: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &i64_def, tk);

		isz i = 0;

		u64 iv = 0;
		if (tk->str.len > 2 && tk->str.str[0] == '0') {
			switch (tk->str.str[1]) {
			case 'x': case 'X': // Parse hexadecimal integer
				i += 2;
				while (i < tk->str.len) {
					iv <<= 4;
					iv |= digit_uint_val(tk->str.str[i++]);
				}
				new->type = &u64_def;
				break;

			case 'b': case 'B': // Parse binary integer
				i += 2;
				while (i < tk->str.len) {
					iv <<= 1;
					iv |= tk->str.str[i++] - '0';
				}
				new->type = &u64_def;
				break;

			case '.':
				i += 2;
				goto parse_float;

			default:
				break;
			}
		}
		else {
			while (i < tk->str.len) { // Parse decimal integer
				char c = tk->str.str[i++];

				switch (c) {
				case '.': parse_float: {
					u64 low = 0;
					u64 mult = 1;
					while (i < tk->str.len) {
						mult *= 10;
						low *= 10;
						low += tk->str.str[i++] - '0';
					}

					new->float_val = (f64)iv + ((f64)low / (f64)mult);
					new->type = &f64_def;
					new->stype = EXPR_FLOAT;
				}	return new;

				case 'u': case 'U':
					new->uint_val = iv;
					new->type = &u64_def;
					new->stype = EXPR_INTEGER;
					return new;

				case 'i': case 'I':
					new->int_val = iv;
					new->type = &i64_def;
					new->stype = EXPR_INTEGER;
					return new;

				case 'f': case 'F':
					new->float_val = (f64)iv;
					new->type = &f64_def;
					new->stype = EXPR_FLOAT;
					return new;

				case 'g': case 'G':
					iv *= 1024;
				case 'm': case 'M':
					iv *= 1024;
				case 'k': case 'K':
					new->int_val = iv * 1024;
					new->type = &i64_def;
					new->stype = EXPR_INTEGER;
					return new;
				}

				iv *= 10;
				iv += c - '0';
			}
		}

		new->uint_val = iv;
		return new;
	}

	case TK_CHAR: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &u8_def, tk);

		char* str = lt_arena_reserve(cx->arena, 0);
		usz len = unescape_str(str, tk);

		if (len > 1)
			ferr("character literal exceeds max length", *tk);
		else if (!len)
			ferr("empty character literal", *tk);

		new->int_val = str[0];
		return new;
	}

	case TK_STRING: consume(cx); {
		char* data = lt_arena_reserve(cx->arena, 0);
		usz len = unescape_str(data, tk);
		lt_arena_reserve(cx->arena, len);

		type_t* type = lt_arena_reserve(cx->arena, sizeof(type_t));
		*type = TYPE(TP_ARRAY, &u8_def);
		type->child_count = len;

		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_STRING, type, tk);
		new->str_val = LSTR(data, len);
		return new;
	}

	case TK_KW_NULL: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &void_ptr_def, tk);
		new->uint_val = 0;
		return new;
	}

	case TK_KW_SIZEOF: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &i64_def, tk);
		consume_type(cx, TK_LEFT_PARENTH, CLSTR(", expected "A_BOLD"'('"A_RESET" after "A_BOLD"'sizeof'"A_RESET));
		new->int_val = type_bytes(parse_type(cx));
		consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"')'"A_RESET));
		return new;
	}

	case TK_KW_ALIGNOF: consume(cx); {
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_INTEGER, &i64_def, tk);
		consume_type(cx, TK_LEFT_PARENTH, CLSTR(", expected "A_BOLD"'('"A_RESET" after "A_BOLD"'sizeof'"A_RESET));
		new->int_val = type_align(parse_type(cx));
		consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"')'"A_RESET));
		return new;
	}

	case TK_KW_SYSCALL: consume(cx); {
		consume_type(cx, TK_LEFT_PARENTH, CLSTR(", expected "A_BOLD"'('"A_RESET));
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(EXPR_SYSCALL, &i64_def, tk);

		expr_t** eit = &new->child_1;
		while (peek(cx, 0)->stype != TK_RIGHT_PARENTH) {
			if (eit != &new->child_1)
				consume_type(cx, TK_COMMA, CLSTR(", expected "A_BOLD"','"A_RESET" or "A_BOLD"')'"A_RESET));

			*eit = parse_expr(cx, NULL);
			eit = &(*eit)->next;
		}
		consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"';'"A_RESET" after syscall"));
		return new;
	}

	case TK_IDENTIFIER: {
		sym_t* sym = symtab_find(cx->symtab, tk->str);
		if (!sym)
			goto undeclared;

		if (sym->stype == SYM_VAR) {
			consume(cx);
			expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*new = EXPR(EXPR_SYM, sym->type, tk);
			sym->flags |= SYMFL_ACCESSED;
			new->sym = sym;
			return new;
		}
		if (sym->stype == SYM_TYPE) {
			type_t* init_type = parse_type(cx);

			tk_t* tk = peek(cx, 0);
			if (tk->stype == TK_COLON) {
				consume(cx);
				expr_t* expr = parse_expr_unary(cx, NULL, 0xFFFF);
				if (!type_convert_explicit(cx, init_type, &expr))
					ferr("cannot convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
							type_to_reserved_str(cx->arena, expr->type), type_to_reserved_str(cx->arena, init_type));
				return expr;
			}

			expr_t* new = parse_expr_primary(cx, init_type);
			if (type && !type_convert_implicit(cx, type, &new))
				ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
						type_to_reserved_str(cx->arena, new->type), type_to_reserved_str(cx->arena, type));
			return new;
		}

	undeclared:
		ferr("use of undeclared identifier "A_BOLD"'%S'"A_RESET, *tk, tk->str);
	}

	default:
		ferr("unexpected token "A_BOLD"'%S'"A_RESET" in expression", *tk, tk->str);
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
expr_t* parse_expr_unary_sfx(parse_ctx_t* cx, type_t* type, int precedence) {
	tk_t* tk = peek(cx, 0);
	expr_t* operand = parse_expr_primary(cx, type);

	operator_t* op;
	while ((op = find_sfx_operator(peek(cx, 0)->stype)) &&
			(op->precedence < precedence || (op->precedence == precedence && op->associate == OP_ASSOC_RIGHT)))
	{
		if (operand->type->stype == TP_VOID)
			ferr(A_BOLD"'void'"A_RESET" type in expression", *tk);

		tk_t* tk = consume(cx);

		if (op->expr == EXPR_MEMBER) {
			// Dereference until the type is no longer a pointer
			tk_t* member_name_tk = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected member name"));
			type_t* it = operand->type;
			while (it->stype == TP_PTR) {
				expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
				*new = EXPR(EXPR_DEREFERENCE, it->base, tk);
				new->child_1 = operand;
				operand = new;

				it = it->base;
			}

			lstr_t member_name = member_name_tk->str;

			// Find 'data' or 'count' member if the type is an array
			if (it->stype == TP_ARRAY_VIEW || it->stype == TP_ARRAY) {
				expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));

				if (lt_lstr_eq(member_name, CLSTR("data"))) {
					type_t* type = lt_arena_reserve(cx->arena, sizeof(type_t));
					*type = TYPE(TP_PTR, it->base);
					*new = EXPR(EXPR_DATA, type, tk);
				}
				else if (lt_lstr_eq(member_name, CLSTR("count")))
					*new = EXPR(EXPR_COUNT, &u64_def, tk);
				else
					ferr(A_BOLD"'%S'"A_RESET" has no member named "A_BOLD"'%S'"A_RESET, *tk,
						type_to_reserved_str(cx->arena, it), member_name);

				new->child_1 = operand;
				operand = new;
				continue;
			}

			if (it->stype != TP_STRUCT)
				ferr("cannot use "A_BOLD"'.'"A_RESET" operator on a non-structure type", *tk);

			usz member_count = it->child_count;
			usz member_index;
			for (usz i = 0; i < member_count; ++i) {
				if (lt_lstr_eq(it->child_names[i], member_name)) {
					member_index = i;
					goto member_found;
				}
			}

			ferr("structure has no member named "A_BOLD"'%S'"A_RESET, *member_name_tk, member_name_tk->str);

		member_found:
			expr_t* member = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*member = EXPR(EXPR_MEMBER, it->children[member_index], tk);
			member->child_1 = operand;
			member->member_index = member_index;
			operand = member;
		}
		else if (op->expr == EXPR_SUBSCRIPT) {
			if (operand->type->stype != TP_PTR && operand->type->stype != TP_ARRAY && operand->type->stype != TP_ARRAY_VIEW)
				ferr("subscripted type "A_BOLD"'%S'"A_RESET" is neither an array nor a pointer",
						*tk, type_to_reserved_str(cx->arena, operand->type));

			expr_t* subscript = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*subscript = EXPR(EXPR_SUBSCRIPT, operand->type->base, tk);
			subscript->child_1 = operand;

			// Insert EXPR_DATA node if the type is an array
			if (operand->type->stype == TP_ARRAY || operand->type->stype == TP_ARRAY_VIEW) {
				type_t* ptr_type = lt_arena_reserve(cx->arena, sizeof(type_t));
				*ptr_type = TYPE(TP_PTR, operand->type->base);

				expr_t* data_ptr = lt_arena_reserve(cx->arena, sizeof(expr_t));
				*data_ptr = EXPR(EXPR_DATA, ptr_type, tk);
				data_ptr->child_1 = operand;
				subscript->child_1 = data_ptr;
			}

			subscript->child_2 = parse_expr(cx, NULL);
			if (is_int(subscript->child_2->type))
				type_convert_implicit(cx, &i64_def, &subscript->child_2);
			else if (is_uint(subscript->child_2->type))
				type_convert_implicit(cx, &u64_def, &subscript->child_2);
			else
				ferr("array index must be an integer", *subscript->child_2->tk);

			if (peek(cx, 0)->stype == TK_DOUBLE_DOT) {
				type_t* view_type = lt_arena_reserve(cx->arena, sizeof(type_t));
				*view_type = TYPE(TP_ARRAY_VIEW, operand->type->base);
				subscript->type = view_type;
				subscript->stype = EXPR_VIEW;

				consume(cx);

				expr_t* next = parse_expr(cx, NULL);
				if (!is_int_any_sign(next->type))
					ferr("array index must be an integer", *next->tk);
				type_convert_implicit(cx, &u64_def, &next);

				subscript->child_2->next = next;
			}
			else {
				if (operand->type->base->stype == TP_VOID)
					ferr("cannot dereference a void pointer", *tk);
			}

			consume_type(cx, TK_RIGHT_BRACKET, CLSTR(", expected "A_BOLD"']'"A_RESET" after array subscript"));

			operand = subscript;
		}
		else if (op->expr == EXPR_CALL) {
			if (operand->type->stype != TP_FUNC)
				ferr("called type "A_BOLD"'%S'"A_RESET" is not a function", *tk,
						type_to_reserved_str(cx->arena, operand->type));

			expr_t* call = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*call = EXPR(EXPR_CALL, operand->type->base, tk);
			call->child_1 = operand;

			expr_t** current_arg = &call->child_2;

			type_t** arg_types = operand->type->children;
			usz arg_i = 0, arg_count = operand->type->child_count;

			lstr_t func_name = CLSTR("function");
			if (operand->stype == EXPR_SYM)
				func_name = operand->sym->name;

			while (peek(cx, 0)->stype != TK_RIGHT_PARENTH) {
				if (current_arg != &call->child_2)
					consume_type(cx, TK_COMMA, CLSTR(", expected "A_BOLD"','"A_RESET" or "A_BOLD"')'"A_RESET));

				if (arg_i == arg_count)
					ferr("too many arguments to "A_BOLD"'%S'"A_RESET", expected %uq", *tk, func_name, arg_count);
				expr_t* arg = parse_expr(cx, arg_types[arg_i]);
				if (!type_convert_implicit(cx, arg_types[arg_i], &arg))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
							type_to_reserved_str(cx->arena, arg->type),
							type_to_reserved_str(cx->arena, arg_types[arg_i]));
				*current_arg = arg;
				current_arg = &arg->next;
				++arg_i;
			}
			if (arg_i != arg_count)
				ferr("too few arguments to "A_BOLD"'%S'"A_RESET", expected %uq", *tk, func_name, arg_count);
			consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"')'"A_RESET" after function call"));

			operand = call;
		}
		else {
			expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
			*new = EXPR(op->expr, operand->type, tk);
			new->child_1 = operand;
			operand = new;

			if (!is_number(new->type))
				ferr("operand of unary "A_BOLD"'%S'"A_RESET" must be a valid number", *new->tk, new->tk->str);
		}
	}

	return operand;
}

expr_t* parse_expr_unary(parse_ctx_t* cx, type_t* type, int precedence) {
	tk_t* tk = peek(cx, 0);

	operator_t* op = find_pfx_operator(tk->stype);
	if (op) {
		consume(cx);
		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(op->expr, NULL, tk);

		expr_t* child = parse_expr_unary(cx, type, precedence);
		new->child_1 = child;

		switch (op->expr) {
		case EXPR_DEREFERENCE:
			if (child->type->stype != TP_PTR)
				ferr("dereferenced type "A_BOLD"'%S'"A_RESET" is not a pointer", *tk,
						type_to_reserved_str(cx->arena, child->type));
			if (child->type->base->stype == TP_VOID)
				ferr("cannot dereference a void pointer", *tk);
			new->type = child->type->base;
			break;

		case EXPR_REFERENCE: {
			type_t* type = lt_arena_reserve(cx->arena, sizeof(type_t));
			*type = TYPE(TP_PTR, child->type);
			if (child->stype == EXPR_SYM)
				child->sym->flags |= SYMFL_REFERENCED;
			new->type = type;
		}	break;

		case EXPR_LOGIC_NOT: new->type = &u8_def; break;

		default:
			new->type = child->type;
			if (!is_number(new->type))
				ferr("operand of unary "A_BOLD"'%S'"A_RESET" must be a valid number", *new->tk, new->tk->str);
			break;
		}
		return new;
	}
	return parse_expr_unary_sfx(cx, type, precedence);
}

static
operator_t* find_binary_operator(tk_stype_t tk_type) {
	if (operators[tk_type].tk != 0xFFFFFFFF)
		return &operators[tk_type];
	return NULL;
}

expr_t* parse_expr_binary(parse_ctx_t* cx, type_t* type, int precedence) {
	expr_t* left = parse_expr_unary(cx, type, precedence);
	tk_t* tk = peek(cx, 0);

	operator_t* op = find_binary_operator(tk->stype);
	while (op && (op->precedence < precedence || (op->precedence == precedence && op->associate == OP_ASSOC_RIGHT))) {
		consume(cx);
		expr_t* right = parse_expr_binary(cx, NULL, op->precedence);
		type_t* type = left->type;

		type = type_make_compatible(cx, tk, op->expr, &left, &right);

		expr_t* new = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*new = EXPR(op->expr, type, tk);
		new->child_1 = left;
		new->child_2 = right;

		if (op->expr == EXPR_UFCS) {
			consume_type(cx, TK_LEFT_PARENTH, CLSTR(", expected "A_BOLD"'('"A_RESET));

			expr_t* obj = new->child_1;
			expr_t* func = new->child_2;

			new->stype = EXPR_CALL;
			new->child_1 = func;

			type_t** arg_types = func->type->children;
			usz arg_i = 1, arg_count = func->type->child_count;

			lstr_t func_name = CLSTR("function");
			if (func->stype == EXPR_SYM)
				func_name = func->sym->name;

			if (!arg_count)
				goto too_many_args;

			if (!type_convert_implicit(cx, arg_types[0], &obj))
				ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
						type_to_reserved_str(cx->arena, obj->type), type_to_reserved_str(cx->arena, arg_types[0]));
			new->child_2 = obj;

			expr_t** current_arg = &obj->next;

			while (peek(cx, 0)->stype != TK_RIGHT_PARENTH) {
				if (current_arg != &obj->next)
					consume_type(cx, TK_COMMA, CLSTR(", expected "A_BOLD"','"A_RESET" or "A_BOLD"')'"A_RESET));

				expr_t* arg = parse_expr(cx, arg_types[arg_i]);
				if (arg_i == arg_count) {
				too_many_args:
					ferr("too many arguments to "A_BOLD"'%S'"A_RESET", expected %uq", *tk, func_name, arg_count);
				}
				if (!type_convert_implicit(cx, arg_types[arg_i], &arg))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
							type_to_reserved_str(cx->arena, arg->type),
							type_to_reserved_str(cx->arena, arg_types[arg_i]));
				*current_arg = arg;
				current_arg = &arg->next;
				++arg_i;
			}
			if (arg_i != arg_count)
				ferr("too few arguments to "A_BOLD"'%S'"A_RESET", expected %uq", *tk, func_name, arg_count);
			consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"')'"A_RESET" after function call"));
		}

		left = new;

		tk = peek(cx, 0);
		op = find_binary_operator(tk->stype);
	}

	return left;
}

expr_t* parse_expr(parse_ctx_t* cx, type_t* type) {
	return parse_expr_binary(cx, type, 100);
}

