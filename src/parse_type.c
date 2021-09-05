#include "parse.h"
#include "parse_helpers.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "type.h"
#include "symtab.h"

#include <lt/str.h>

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
		*struc = type_make(TP_STRUCT);
		type_t** member = &struc->child;
		consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected '{' after 'struct' keyword\n"));

		while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
			tk_t* tk = peek(cx, 0);

			type_t* new = parse_type(cx);
			if (!new)
				lt_ferrf("%s:%uz: Unexpected token '%S', expected member type\n", cx->path, tk->line_index + 1, tk->str);
			new->name = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected member name\n"))->str;
			*member = new;
			member = &new->next;

			consume_type(cx, TK_SEMICOLON, CLSTR(", expected ';' after member name\n"));
		}

		consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected '}' after struct members\n"));
		base = struc;
	}	break;

	default: unexpected_tk:
		lt_ferrf("%s:%uz: Unexpected token '%S', expected a valid type\n", cx->path, tk.line_index + 1, tk.str);
	}

	for (;;) {
		tk = *peek(cx, 0);
		switch (tk.stype) {
		case TK_LEFT_PARENTH: consume(cx); {
			type_t* new = lt_arena_reserve(cx->arena, sizeof(type_t));
			*new = type_make(TP_FUNC);
			new->base = base;
			consume_type(cx, TK_RIGHT_PARENTH, CLSTR(", expected ')'\n"));
			base = new;
		}	break;

		case TK_ASTERISK: consume(cx); {
			type_t* new = lt_arena_reserve(cx->arena, sizeof(type_t));
			*new = type_make(TP_PTR);
			new->base = base;
			base = new;
			break;
		}

		case TK_LEFT_BRACKET: consume(cx); {
			type_t* new = lt_arena_reserve(cx->arena, sizeof(type_t));

			if (peek(cx, 0)->stype == TK_RIGHT_BRACKET)
				*new = type_make(TP_ARRAY_VIEW);
			else {
				usz line_index = peek(cx, 0)->line_index;
				expr_t* expr = parse_expr(cx, NULL);
				if (!is_int_any_sign(expr->type))
					lt_ferrf("%s:%uz: Array index must be an integer type\n", cx->path, line_index);
				*new = type_make(TP_ARRAY);
			}

			consume_type(cx, TK_RIGHT_BRACKET, CLSTR(", expected ']' after array type\n"));

			new->base = base;
			base = new;
			break;
		}

		default:
			return base;
		}
	}
}

