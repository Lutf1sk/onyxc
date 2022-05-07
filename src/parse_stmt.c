#include "parse.h"
#include "parse_helpers.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "tk.h"
#include "type.h"
#include "symtab.h"
#include "gen.h"
#include "segment.h"

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
			ferr("invalid redefinition of "A_BOLD"'%S'"A_RESET"", *ident_tk, ident_tk->str);

		type_t* type = init_type;
		tk_t* tk = peek(cx, 0);
		if (tk->stype == TK_DOUBLE_COLON || tk->stype == TK_EQUAL) {
			consume(cx);
			if (tk->stype == TK_DOUBLE_COLON)
				flags |= SYMFL_CONST;

			new->expr = parse_expr(cx, init_type);
			if (init_type) {
				if (!type_convert_implicit(cx, init_type, &new->expr))
					ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *ident_tk,
							type_to_reserved_str(cx->arena, new->expr->type), type_to_reserved_str(cx->arena, init_type));
			}
			else
				type = new->expr->type;
		}
		else if (!type)
			ferr("variable with implicit type must be initialized", *ident_tk);

		if (type->stype == TP_VOID)
			ferr("variable cannot be of type "A_BOLD"'void'"A_RESET, *ident_tk);

		sym->expr = new->expr;
		sym->type = type;
		new->type = type;
		sym->flags = flags;
		symtab_insert(cx->symtab, ident_tk->str, sym);

		if (flags & SYMFL_CONST) {
			sym->val = gen_const_expr(cx->gen_cx, sym->expr);
			if ((sym->val.stype & ~IVAL_REF) == IVAL_SEG)
				cx->gen_cx->seg[sym->val.uint_val].name = sym->name;
		}
		else if (flags & SYMFL_GLOBAL) {
			usz size = type_bytes(sym->type);
			void* data = lt_arena_reserve(cx->arena, size);
			memset(data, 0, size);
			sym->val = IVAL(IVAL_SEG | IVAL_REF, .uint_val = new_data_seg(cx->gen_cx, SEG_ENT(SEG_DATA, sym->name, size, data)));

			if (sym->expr) {
				ival_t v = gen_const_expr(cx->gen_cx, sym->expr);
				ival_write_comp(cx->gen_cx, sym->expr->type, v, data);
			}
		}

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

	case TK_KW_IMPORT: consume(cx);
		lex_ctx_t* old_lex_cx = cx->lex;
		stmt_t* stmt = NULL;
 		stmt_t** it = &stmt;

		for (;;) {
			tk_t* path_tk = consume_type(cx, TK_STRING, CLSTR(", expected a path after 'import'"));
			char* path = lt_arena_reserve(cx->arena, 0);
			usz len = unescape_str(path, path_tk);
			lt_arena_reserve(cx->arena, len + 1);
			path[len] = 0;
			LT_ASSERT(len < 4096);

			// TODO: "include guards"

			cx->lex = lex_file(cx->arena, path, path_tk);
			*it = parse(cx);
			cx->lex = old_lex_cx;

			if (peek(cx, 0)->stype != TK_COMMA) {
				consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after import statement"));
				return stmt;
			}
			consume(cx);
			it = &(*it)->child;
		}

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
				ferr("invalid redefinition of "A_BOLD"'%S'"A_RESET, *ident_tk, ident_tk->str);

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
		if (ret_type->stype != TP_VOID) {
			new->expr = parse_expr(cx, NULL);
			if (!type_convert_implicit(cx, ret_type, &new->expr))
				ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, tk,
						type_to_reserved_str(cx->arena, new->expr->type),
						type_to_reserved_str(cx->arena, ret_type));
		}
		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after "A_BOLD"'return'"A_RESET));
		return new;
	}

	case TK_KW_IF: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_IF);
		new->expr = parse_expr(cx, NULL);

		if (!is_number(new->expr->type))
			ferr("result of "A_BOLD"'if'"A_RESET" condition must be a valid number", tk);

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

		if (!is_number(new->expr->type))
			ferr("result of "A_BOLD"'while'"A_RESET" condition must be a valid number", tk);
		new->child = parse_compound(cx);
		return new;
	}

	case TK_KW_FOR: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		PUSH_SCOPE();

		type_t* it_type = parse_type(cx);

		tk_t* tk = peek(cx, 0);
		if (!is_int_any_sign(it_type))
			ferr("for loop iterator must be an integer", *tk);

		tk_t ident_tk = *consume_type(cx, TK_IDENTIFIER, CLSTR(", expected identifier"));

		expr_t* init = lt_arena_reserve(cx->arena, sizeof(expr_t));
		*init = EXPR(EXPR_INTEGER, it_type, tk);
		init->uint_val = 0;

		sym_t* sym = lt_arena_reserve(cx->arena, sizeof(sym_t));
		*sym = SYM(SYM_VAR, ident_tk.str);
		sym->type = it_type;
		sym->expr = init;
		sym->flags = SYMFL_ACCESSED;
		symtab_insert(cx->symtab, ident_tk.str, sym);

		consume_type(cx, TK_DOUBLE_DOT, CLSTR(", expected "A_BOLD"'..'"A_RESET));

		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_FOR);

		tk = peek(cx, 0);
		new->expr = parse_expr(cx, NULL);
		if (!type_convert_implicit(cx, it_type, &new->expr))
			ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%s'"A_RESET, *tk,
					type_to_reserved_str(cx->arena, new->expr->type), type_to_reserved_str(cx->arena, it_type));

		new->sym = sym;
		new->child = parse_compound(cx);

		POP_SCOPE();

		return new;
	}

	case TK_KW_STRUCT: {
		return parse_let(cx, parse_type(cx));
	}	break;

	default:
		stmt_t* new = lt_arena_reserve(cx->arena, sizeof(stmt_t));
		type_t* type = NULL;

		if (tk.stype == TK_IDENTIFIER) {
			sym_t* sym = symtab_find(cx->symtab, tk.str);
			if (!sym)
				ferr("use of undeclared identifier "A_BOLD"'%S'"A_RESET, tk, tk.str);

			if (sym->stype == SYM_TYPE) {
				type = parse_type(cx);
				tk_stype_t ntk = peek(cx, 0)->stype;
				if (ntk == TK_IDENTIFIER)
					return parse_let(cx, type);
				else if (ntk == TK_COLON)
					consume(cx);
			}
		}

		if (!cx->curr_func_type)
			goto outside_func;
		*new = STMT(STMT_EXPR);
		new->expr = parse_expr(cx, type);

		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after expression"));
		return new;
	}

outside_func:
	ferr(A_BOLD"'%S'"A_RESET" must be inside a function body", tk, tk.str);
}

