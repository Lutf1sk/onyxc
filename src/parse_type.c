#include "parse.h"
#include "parse_helpers.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "type.h"
#include "symtab.h"
#include "gen.h"

#include <lt/str.h>
#include <lt/io.h>

type_t* parse_type(parse_ctx_t* cx) {
	tk_t tk = *peek(cx, 0);
	type_t* base = NULL;
	switch (tk.stype) {
	case TK_IDENTIFIER: consume(cx); {
		sym_t* sym = symtab_find(cx->symtab, tk.str);
		if (!sym || sym->stype != SYM_TYPE)
			goto unexpected_tk;
		base = sym->type;
	}	break;

	case TK_KW_STRUCT: consume(cx); {
		type_t* struc = lt_arena_reserve(cx->arena, sizeof(type_t));
		*struc = TYPE(TP_STRUCT, NULL);

		consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected "A_BOLD"'{'"A_RESET" after "A_BOLD"'struct'"A_RESET));

		while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
			tk_t* tk = peek(cx, 0);
			type_t* new = parse_type(cx);

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

	for (;;) {
		tk = *peek(cx, 0);
		switch (tk.stype) {
		case TK_LEFT_PARENTH: consume(cx); {
			type_t* func = lt_arena_reserve(cx->arena, sizeof(type_t));
			*func = TYPE(TP_FUNC, base);

			while (peek(cx, 0)->stype != TK_RIGHT_PARENTH) {
				if (func->child_count)
					consume_type(cx, TK_COMMA, CLSTR(", expected "A_BOLD"','"A_RESET" or "A_BOLD"')'"A_RESET));
				type_t* new = parse_type(cx);

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
			type_t* new = lt_arena_reserve(cx->arena, sizeof(type_t));
			*new = TYPE(TP_PTR, base);
			base = new;
			break;
		}

		case TK_LEFT_BRACKET: consume(cx); {
			type_t* new = lt_arena_reserve(cx->arena, sizeof(type_t));

			if (peek(cx, 0)->stype == TK_RIGHT_BRACKET)
				*new = TYPE(TP_ARRAY_VIEW, base);
			else {
				if (base->stype == TP_VOID)
					ferr("fixed-size array cannot be of type "A_BOLD"'void'"A_RESET, tk);

				tk_t* tk = peek(cx, 0);
				expr_t* expr = parse_expr(cx, NULL);
				if (!is_int_any_sign(expr->type))
					ferr("fixed array size must be an integer", *tk);

				*new = TYPE(TP_ARRAY, base);
				ival_t ival = gen_const_expr(cx->gen_cx, expr);
				if (ival.stype != IVAL_IMM)
					ferr("fixed array size must be known at parse-time", *tk);
				new->child_count = ival.uint_val;
				if (is_int(expr->type) && (isz)new->child_count < 0)
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

