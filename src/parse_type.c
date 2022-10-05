#include "parse.h"
#include "parse_helpers.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "type.h"
#include "symtab.h"
#include "gen.h"

#include <lt/str.h>
#include <lt/io.h>

type_t* parse_type(parse_ctx_t* cx, type_t* base) {
	if (base)
		goto parse_postfix;

	tk_t tk = *peek(cx, 0);
	switch (tk.stype) {
	case TK_IDENTIFIER: consume(cx); {
		sym_t* sym = symtab_find(cx->symtab, tk.str);
		if (!sym || sym->stype != SYM_TYPE)
			goto unexpected_tk;
		base = sym->type;
	}	break;

	case TK_KW_ENUM: consume(cx); {
		tk_t* type_tk = peek(cx, 0);

		type_t* type = parse_type(cx, NULL);
		if (type->stype == TP_VOID)
			ferr("enum cannot be of type "A_BOLD"'void'"A_RESET, *type_tk);

		consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected "A_BOLD"'{'"A_RESET" after "A_BOLD"'enum'"A_RESET));

		b8 auto_increment = is_int_any_sign(resolve_enum(type));
		u64 auto_val = 0;

		type_t* parent = lt_amalloc(cx->arena, sizeof(type_t));
		*parent = TYPE(TP_ENUM, type);
		parent->symtab = symtab_create(cx->arena);

		while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
			tk_t* ident_tk = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected identifier"));
			lstr_t ident = ident_tk->str;

			if (!symtab_definable(parent->symtab, ident))
				ferr("invalid redefinition of "A_BOLD"'%S'"A_RESET"", *ident_tk, ident);

			sym_t* sym = lt_amalloc(cx->arena, sizeof(sym_t));
			*sym = SYM(SYM_VAR, ident);
			sym->type = type;
			sym->flags = SYMFL_CONST;

			if (peek(cx, 0)->stype == TK_COLON) {
				consume(cx);
				tk_t* tk = peek(cx, 0);
				expr_t* expr = parse_expr(cx, parent);
				if (!type_convert_implicit(cx, type, &expr))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
							type_to_reserved_str(cx->arena, expr->type), type_to_reserved_str(cx->arena, type));

				sym->val = gen_const_expr(cx->gen_cx, expr);
				if (auto_increment)
					auto_val = sym->val.uint_val + 1;
			}
			else if (auto_increment)
				sym->val = IMMI(auto_val++);
			else
				ferr("non-integer enum values must be explicitly initialized", *ident_tk);

			symtab_insert(parent->symtab, ident, sym);

			if (peek(cx, 0)->stype != TK_COMMA)
				break;
			consume(cx);
		}

		consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected "A_BOLD"'}'"A_RESET));
		base = parent;
	}	break;

	case TK_KW_STRUCT: consume(cx); {
		type_t* struc = lt_amalloc(cx->arena, sizeof(type_t));
		*struc = TYPE(TP_STRUCT, NULL);

		consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected "A_BOLD"'{'"A_RESET" after "A_BOLD"'struct'"A_RESET));

		while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
			tk_t* tk = peek(cx, 0);
			type_t* new = parse_type(cx, NULL);

			if (new->stype == TP_VOID)
				ferr("struct member cannot be of type "A_BOLD"'void'"A_RESET, *tk);

			for (;;) {
				lstr_t name = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected member name"))->str;

				type_add_child(struc, new, name, NULL);

				if (peek(cx, 0)->stype != TK_COMMA)
					break;
				consume(cx);
			}
			consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after member name"));
		}

		consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected "A_BOLD"'}'"A_RESET" after struct members"));
		base = struc;
	}	break;

	default: unexpected_tk:
		ferr("unexpected token "A_BOLD"'%S'"A_RESET", expected a valid type", tk, tk.str);
	}

parse_postfix:
	for (;;) {
		tk = *peek(cx, 0);
		switch (tk.stype) {
		case TK_LEFT_PARENTH: consume(cx); {
			type_t* func = lt_amalloc(cx->arena, sizeof(type_t));
			*func = TYPE(TP_FUNC, base);

			while (peek(cx, 0)->stype != TK_RIGHT_PARENTH) {
				if (func->child_count)
					consume_type(cx, TK_COMMA, CLSTR(", expected "A_BOLD"','"A_RESET" or "A_BOLD"')'"A_RESET));
				type_t* new = parse_type(cx, NULL);

				if (new->stype == TP_VOID)
					ferr("parameter cannot have type "A_BOLD"'void'"A_RESET, tk);

				lstr_t name = NLSTR();
				if (peek(cx, 0)->stype == TK_IDENTIFIER)
					 name = consume(cx)->str;

				type_add_child(func, new, name, NULL);
			}

			consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected "A_BOLD"')'"A_RESET));
			base = func;
		}	break;

		case TK_ASTERISK: consume(cx); {
			type_t* new = lt_amalloc(cx->arena, sizeof(type_t));
			*new = TYPE(TP_PTR, base);
			base = new;
			break;
		}

		case TK_LEFT_BRACKET: consume(cx); {
			type_t* new = lt_amalloc(cx->arena, sizeof(type_t));

			if (peek(cx, 0)->stype == TK_RIGHT_BRACKET)
				*new = TYPE(TP_ARRAY_VIEW, base);
			else {
				if (base->stype == TP_VOID)
					ferr("fixed-size array cannot be of type "A_BOLD"'void'"A_RESET, tk);

				tk_t* tk = peek(cx, 0);
				expr_t* expr = parse_expr(cx, NULL);
				if (!is_int_any_sign(resolve_enum(expr->type)))
					ferr("fixed array size must be an integer", *tk);

				*new = TYPE(TP_ARRAY, base);
				ival_t ival = gen_const_expr(cx->gen_cx, expr);
				if (ival.stype != IVAL_IMM)
					ferr("fixed array size must be known at parse-time", *tk);
				new->child_count = ival.uint_val;
				if (is_int(resolve_enum(expr->type)) && (isz)new->child_count < 0)
					ferr("fixed array size cannot be negative", *tk);
			}

			consume_type(cx, TK_RIGHT_BRACKET, CLSTR(", expected "A_BOLD"']'"A_RESET" after array size"));
			base = new;
			break;
		}

		default:
			return base;
		}
	}
}

type_t* try_parse_type(parse_ctx_t* cx) {
	tk_t* tk = peek(cx, 0);

	switch (tk->stype) {
	case TK_IDENTIFIER: {
		sym_t* sym = try_parse_sym(cx, SYM_TYPE);
		if (!sym)
			return NULL;
		return parse_type(cx, sym->type);
	}
	case TK_KW_STRUCT: case TK_KW_ENUM:
		return parse_type(cx, NULL);
	}

	return NULL;
}

