#include "parse.h"
#include "parse_helpers.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "tk.h"
#include "type.h"
#include "symtab.h"

#include <lt/str.h>

#define PUSH_SCOPE() { \
	symtab_t* new_symtab = lt_arena_reserve(cx->arena, sizeof(symtab_t)); \
	memset(new_symtab, 0, sizeof(symtab_t)); \
	new_symtab->parent = cx->symtab; \
	cx->symtab = new_symtab; \
}

#define POP_SCOPE() (cx->symtab = cx->symtab->parent)

stmt_t* parse_func_body(parse_ctx_t* cx) {
	consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected compound statement ('{')\n"));

	PUSH_SCOPE();

	type_t** param_types = cx->curr_func_type->children;
	lstr_t* param_names = cx->curr_func_type->child_names;
	usz param_count = cx->curr_func_type->child_count;
	for (usz i = 0; i < param_count; ++i) {
		sym_t* sym = lt_arena_reserve(cx->arena, sizeof(sym_t));
		*sym = SYM(SYM_VAR, param_names[i]);
		sym->type = param_types[i];

		symtab_insert(cx->symtab, param_names[i], sym);
	}

	stmt_t* root = lt_arena_reserve(cx->arena, sizeof(stmt_t));
	*root = STMT(STMT_COMPOUND);

	stmt_t** current = &root->child;
	while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
		stmt_t* new = parse_stmt(cx);
		*current = new;
		current = &new->next;
	}

	POP_SCOPE();
	consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected closing brace\n"));
	return root;
}

stmt_t* parse_compound(parse_ctx_t* cx) {
	consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected compound statement ('{')\n"));

	PUSH_SCOPE();

	stmt_t* root = lt_arena_reserve(cx->arena, sizeof(stmt_t));
	*root = STMT(STMT_COMPOUND);

	stmt_t** current = &root->child;
	while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
		stmt_t* new = parse_stmt(cx);
		*current = new;
		current = &new->next;
	}

	POP_SCOPE();
	consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected closing brace\n"));
	return root;
}

stmt_t* parse_let(parse_ctx_t* cx, type_t* type) {
	stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
	*new = STMT(STMT_LET);
	new->identifier = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected variable name\n"))->str;

	b8 constant = 0;
	b8 initialized = 0;

	tk_t* tk = peek(cx, 0);
	if (tk->stype == TK_EQUAL)
		initialized = 1;
	else if (tk->stype == TK_DOUBLE_COLON) {
		constant = 1;
		initialized = 1;
	}

	if (initialized) {
		consume(cx);
		new->expr = parse_expr(cx, type);
		if (type && !type_convert_implicit(cx, type, &new->expr))
			lt_ferrf("%s:%uz: Cannot implicitly convert %S to %S\n", cx->path, tk->line_index + 1,
					type_to_reserved_str(cx->arena, new->expr->type), type_to_reserved_str(cx->arena, type));
		new->type = new->expr->type;
	}
	else if (!type)
		lt_ferrf("%s:%uz: 'let' with implicit type must be initialized\n", cx->path, tk->line_index + 1, tk->str);

	if (type)
		new->type = type;

	sym_t* sym = lt_arena_reserve(cx->arena, sizeof(sym_t));
	*sym = SYM(SYM_VAR, new->identifier);
	sym->type = new->type;
	sym->expr = new->expr;
	if (constant)
		sym->flags |= SYMFL_CONST;
	symtab_insert(cx->symtab, new->identifier, sym);

	consume_type(cx, TK_SEMICOLON, CLSTR(", expected ';' after variable definition\n"));
	return new;
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
		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_DEF);
		new->identifier = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected type name\n"))->str;

		consume_type(cx, TK_DOUBLE_COLON, CLSTR(", expected '::' after type name\n"));
		type_t* type = parse_type(cx);
		if (!type)
			lt_ferrf("%s:%uz: Expected a type\n", cx->path, peek(cx, 0)->line_index);

		sym_t* sym = lt_arena_reserve(cx->arena, sizeof(sym_t));
		*sym = SYM(SYM_TYPE, new->identifier);
		sym->type = type;
		symtab_insert(cx->symtab, new->identifier, sym);

		consume_type(cx, TK_SEMICOLON, CLSTR(", expected ';' after type definition\n"));
		new->type = type;
		return new;
	}

	case TK_KW_RETURN: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		type_t* ret_type = cx->curr_func_type->base;

		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_RETURN);
		new->expr = parse_expr(cx, NULL);
		if (!type_convert_implicit(cx, ret_type, &new->expr))
			lt_ferrf("%s:%uz: Cannot implicitly convert %S to %S\n", cx->path, tk.line_index + 1,
					type_to_reserved_str(cx->arena, new->expr->type),
					type_to_reserved_str(cx->arena, ret_type));
		consume_type(cx, TK_SEMICOLON, CLSTR(", expected ';' after 'return'\n"));
		return new;
	}

	default:
		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));

		if (tk.stype == TK_IDENTIFIER) {
			sym_t* sym = symtab_find(cx->symtab, tk.str);
			if (!sym)
				lt_ferrf("%s:%uz: Use of undeclared identifier '%S'\n", cx->path, tk.line_index + 1, tk.str);

			if (sym->stype == SYM_TYPE)
				return parse_let(cx, parse_type(cx));
		}
		*new = STMT(STMT_EXPR);
		new->expr = parse_expr(cx, NULL);

		consume_type(cx, TK_SEMICOLON, CLSTR(", expected ';' after expression statement\n"));
		return new;
	}

outside_func:
	lt_ferrf("%s:%uz: '%S' must be inside a function body\n", cx->path, tk.line_index + 1, tk.str);
}

