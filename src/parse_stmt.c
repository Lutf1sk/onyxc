#include "parse.h"
#include "parse_helpers.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "tk.h"
#include "type.h"
#include "symtab.h"

#include <lt/str.h>
#include <lt/io.h>

#define PUSH_SCOPE() { \
	symtab_t* new_symtab = lt_arena_reserve(cx->arena, sizeof(symtab_t)); \
	memset(new_symtab->counts, 0, sizeof(new_symtab->counts)); \
	new_symtab->parent = cx->symtab; \
	cx->symtab = new_symtab; \
}

#define POP_SCOPE() (cx->symtab = cx->symtab->parent)

stmt_t* parse_func_body(parse_ctx_t* cx) {
	consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected "A_BOLD"'{'"A_RESET));
	PUSH_SCOPE();
	type_t** param_types = cx->curr_func_type->children;
	lstr_t* param_names = cx->curr_func_type->child_names;
	sym_t** param_syms = cx->curr_func_type->child_syms;
	usz param_count = cx->curr_func_type->child_count;
	for (usz i = 0; i < param_count; ++i) {
		if (!param_names[i].str)
			continue;

		sym_t* sym = lt_arena_reserve(cx->arena, sizeof(sym_t));
		*sym = SYM(SYM_VAR, param_names[i]);
		sym->type = param_types[i];
		sym->flags |= SYMFL_ARG;

		symtab_insert(cx->symtab, param_names[i], sym);
		param_syms[i] = sym;
	}

	stmt_t* root = lt_arena_reserve(cx->arena, sizeof(stmt_t));
	*root = STMT(STMT_COMPOUND);

	stmt_t** current = &root->child;
	while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
		stmt_t* new = parse_stmt(cx);
		if (!new)
			continue;
		*current = new;
		current = &new->next;
	}

	POP_SCOPE();
	consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected "A_BOLD"'}'"A_RESET));
	return root;
}

stmt_t* parse_compound(parse_ctx_t* cx) {
	consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected "A_BOLD"'{'"A_RESET));

	PUSH_SCOPE();

	stmt_t* root = lt_arena_reserve(cx->arena, sizeof(stmt_t));
	*root = STMT(STMT_COMPOUND);

	stmt_t** current = &root->child;
	while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
		stmt_t* new = parse_stmt(cx);
		if (!new)
			continue;
		*current = new;
		current = &new->next;
	}

	POP_SCOPE();
	consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected "A_BOLD"'}'"A_RESET));
	return root;
}

stmt_t* parse_let(parse_ctx_t* cx, type_t* init_type) {
	stmt_t* stmt = NULL;
	stmt_t** it = &stmt;

	for (;;) {
		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_LET);
		tk_t* ident_tk = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected variable name"));

		sym_t* sym = lt_arena_reserve(cx->arena, sizeof(sym_t));
		*sym = SYM(SYM_VAR, ident_tk->str);

		new->sym = sym;

		u8 flags = 0;
		if (!cx->curr_func_type)
			flags |= SYMFL_GLOBAL;

		if (!symtab_definable(cx->symtab, ident_tk->str))
			ferr("invalid redefinition of "A_BOLD"'%S'"A_RESET"", cx->lex, *ident_tk, ident_tk->str);

		type_t* type = init_type;
		tk_t* tk = peek(cx, 0);
		if (tk->stype == TK_DOUBLE_COLON || tk->stype == TK_EQUAL) {
			consume(cx);
			if (tk->stype == TK_DOUBLE_COLON)
				flags |= SYMFL_CONST;

			new->expr = parse_expr(cx, init_type);
			if (init_type) {
				if (!type_convert_implicit(cx, init_type, &new->expr))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, cx->lex, *ident_tk,
							type_to_reserved_str(cx->arena, new->expr->type), type_to_reserved_str(cx->arena, init_type));
			}
			else
				type = new->expr->type;
		}
		else if (!type)
			ferr("variable with implicit type must be initialized", cx->lex, *ident_tk);

		sym->expr = new->expr;
		sym->type = type;
		new->type = type;
		sym->flags = flags;
		symtab_insert(cx->symtab, ident_tk->str, sym);

		*it = new;
		if (peek(cx, 0)->stype != TK_COMMA) {
			if (type->stype != TP_FUNC)
				consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after variable definition"));
			return stmt;
		}
		consume(cx);
		it = &new->child;
	}
}

stmt_t* parse_stmt(parse_ctx_t* cx) {
	tk_t tk = *peek(cx, 0);
	switch (tk.stype) {
	case TK_SEMICOLON: consume(cx);
		return NULL;

	case TK_LEFT_BRACE:
		return parse_compound(cx);

	case TK_KW_LET: consume(cx);
		return parse_let(cx, NULL);

	case TK_KW_DEF: consume(cx); {
		stmt_t* stmt = NULL;
		stmt_t** it = &stmt;
		for (;;) {
			stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
			*new = STMT(STMT_DEF);
			tk_t* ident_tk = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected type name"));

			if (!symtab_definable(cx->symtab, ident_tk->str))
				ferr("invalid redefinition of "A_BOLD"'%S'"A_RESET, cx->lex, *ident_tk, ident_tk->str);

			consume_type(cx, TK_DOUBLE_COLON, CLSTR(", expected "A_BOLD"'::'"A_RESET" after type name"));

			type_t* type = lt_arena_reserve(cx->arena, sizeof(type_t));

			type_t* copied_type = parse_type(cx);
			*type = *copied_type;

			sym_t* sym = lt_arena_reserve(cx->arena, sizeof(sym_t));
			*sym = SYM(SYM_TYPE, ident_tk->str);
			sym->type = type;
			symtab_insert(cx->symtab, ident_tk->str, sym);

			type->sym = sym;
			new->sym = sym;
			new->type = type;

			*it = new;
			if (peek(cx, 0)->stype != TK_COMMA) {
				if (type->stype != TP_STRUCT)
					consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after type definition"));
				return stmt;
			}
			consume(cx);
			it = &new->child;
		}
	}

	case TK_KW_RETURN: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		type_t* ret_type = cx->curr_func_type->base;

		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_RETURN);
		new->expr = parse_expr(cx, NULL);
		if (!type_convert_implicit(cx, ret_type, &new->expr))
			ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, cx->lex, tk,
					type_to_reserved_str(cx->arena, new->expr->type),
					type_to_reserved_str(cx->arena, ret_type));
		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after "A_BOLD"'return'"A_RESET));
		return new;
	}

	case TK_KW_IF: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_IF);
		new->expr = parse_expr(cx, NULL);

		if (!is_scalar(new->expr->type))
			ferr("result of "A_BOLD"'if'"A_RESET" condition must be scalar", cx->lex, tk);

		new->child = parse_compound(cx);

		if (peek(cx, 0)->stype != TK_KW_ELSE)
			return new;

		consume(cx);
		new->child_2 = parse_compound(cx);
		return new;
	}

	case TK_KW_WHILE: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_WHILE);
		new->expr = parse_expr(cx, NULL);

		if (!is_scalar(new->expr->type))
			ferr("result of "A_BOLD"'while'"A_RESET" condition must be scalar", cx->lex, tk);
		new->child = parse_compound(cx);
		return new;
	}

	case TK_KW_SYSCALL: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_SYSCALL);

		expr_t** eit = &new->expr;
		while (peek(cx, 0)->stype != TK_SEMICOLON) {
			if (eit != &new->expr)
				consume_type(cx, TK_COMMA, CLSTR(", expected "A_BOLD"';'"A_RESET", "A_BOLD"','"A_RESET" or "A_BOLD"')'"A_RESET));

			*eit = parse_expr(cx, NULL);
			eit = &(*eit)->next;
		}
		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after syscall"));
		return new;
	}

	case TK_KW_STRUCT: {
		return parse_let(cx, parse_type(cx));
	}	break;

	default:
		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));

		if (tk.stype == TK_IDENTIFIER) { // TODO: This approach does not allow a line to start with a cast
			sym_t* sym = symtab_find(cx->symtab, tk.str);
			if (!sym)
				ferr("use of undeclared identifier "A_BOLD"'%S'"A_RESET, cx->lex, tk, tk.str);

			if (sym->stype == SYM_TYPE)
				return parse_let(cx, parse_type(cx));
		}

		if (!cx->curr_func_type)
			goto outside_func;
		*new = STMT(STMT_EXPR);
		new->expr = parse_expr(cx, NULL);

		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after expression"));
		return new;
	}

outside_func:
	ferr(A_BOLD"'%S'"A_RESET" must be inside a function body", cx->lex, tk, tk.str);
}

