#include "parse.h"
#include "parse_helpers.h"
#include "stmt_ast.h"
#include "expr_ast.h"
#include "tk.h"
#include "type.h"
#include "symtab.h"
#include "gen.h"
#include "segment.h"
#include "jit.h"

// !!
#include "amd64/amd64.h"

#include <lt/str.h>
#include <lt/io.h>

#define PUSH_SCOPE() { \
	symtab_t* new_symtab = symtab_create(cx->arena); \
	new_symtab->parent = cx->symtab; \
	cx->symtab = new_symtab; \
}

#define POP_SCOPE() (cx->symtab = cx->symtab->parent)

stmt_t* parse_func_body(parse_ctx_t* cx, symtab_t* label_symtab) {
	consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected "A_BOLD"'{'"A_RESET));
	PUSH_SCOPE();

	symtab_t* label_symtab_old = cx->label_symtab;
	cx->label_symtab = label_symtab;
	memset(cx->label_symtab, 0, sizeof(symtab_t));

	type_t** param_types = cx->curr_func_type->children;
	lstr_t* param_names = cx->curr_func_type->child_names;
	sym_t** param_syms = cx->curr_func_type->child_syms;
	usz param_count = cx->curr_func_type->child_count;
	for (usz i = 0; i < param_count; ++i) {
		if (!param_names[i].str)
			continue;

		sym_t* sym = lt_amalloc(cx->arena, sizeof(sym_t));
		*sym = SYM(SYM_VAR, param_names[i]);
		sym->type = param_types[i];
		sym->flags |= SYMFL_ARG;

		symtab_insert(cx->symtab, param_names[i], sym);
		param_syms[i] = sym;
	}

	stmt_t* root = lt_amalloc(cx->arena, sizeof(stmt_t));
	*root = STMT(STMT_COMPOUND);

	stmt_t** current = &root->child;
	while (peek(cx, 0)->stype != TK_RIGHT_BRACE) {
		stmt_t* new = parse_stmt(cx);
		if (!new)
			continue;
		*current = new;
		current = &new->next;
	}

	cx->label_symtab = label_symtab_old;

	POP_SCOPE();
	consume_type(cx, TK_RIGHT_BRACE, CLSTR(", expected "A_BOLD"'}'"A_RESET));
	return root;
}

stmt_t* parse_compound(parse_ctx_t* cx) {
	consume_type(cx, TK_LEFT_BRACE, CLSTR(", expected "A_BOLD"'{'"A_RESET));

	PUSH_SCOPE();

	stmt_t* root = lt_amalloc(cx->arena, sizeof(stmt_t));
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

stmt_t* parse_symdef(parse_ctx_t* cx, lstr_t str) {
	tk_t* tk = consume_type(cx, TK_COLON, CLSTR(", expected "A_BOLD"':'"A_RESET" after name in symbol definition"));

	sym_t* sym = lt_amalloc(cx->arena, sizeof(sym_t));
	*sym = SYM(SYM_VAR, str);

	type_t* type = try_parse_type(cx);

	expr_t* expr = NULL;
	if (peek(cx, 0)->stype == TK_COLON) {
		consume(cx);
		sym->flags |= SYMFL_CONST;

		if (!type) {
			type = try_parse_type(cx);
			if (type && peek(cx, 0)->stype == TK_SEMICOLON) {
				sym->stype = SYM_TYPE;
			}
			else if (!type && peek(cx, 0)->stype == TK_KW_HERE) {
				consume(cx);
				sym->stype = SYM_LABEL;
				symtab_insert(cx->label_symtab, str, sym);
				sym->lbl = cx->label_symtab->count;
			}
			else {
				expr = parse_expr(cx, type);
				type = expr->type;
			}
		}
		else
			expr = parse_expr(cx, type);
	}
	else if (peek(cx, 0)->stype == TK_EQUAL) {
		consume(cx);
		expr = parse_expr(cx, type);
		if (!type)
			type = expr->type;
	}
	else if (type && peek(cx, 0)->stype == TK_SEMICOLON) {}
	else
		ferr("expected "A_BOLD"':'"A_RESET", or "A_BOLD"'='"A_RESET" after symbol definition", *peek(cx, 0));

	sym->type = type;
	sym->expr = expr;

	if (sym->stype == SYM_VAR) {
		LT_ASSERT(type);

		if (sym->flags & SYMFL_CONST) {
			sym->val = gen_const_expr(cx->gen_cx, expr);
			if ((sym->val.stype & ~IVAL_REF) == IVAL_SEG)
				cx->gen_cx->segtab->seg[sym->val.uint_val].name = sym->name;
		}
		else if (!cx->curr_func_type) {
			sym->flags |= SYMFL_GLOBAL;

			usz size = type_bytes(sym->type);
			void* data = lt_amalloc(cx->arena, size);
			memset(data, 0, size);
			u32 seg_i = new_segment(cx->gen_cx->segtab, DATASEG(sym->name, size, data));
			sym->val = IVAL(IVAL_SEG | IVAL_REF, .uint_val = seg_i);

			if (expr) {
				ival_t v = gen_const_expr(cx->gen_cx, expr);
				ival_write_comp(cx->gen_cx, &cx->gen_cx->segtab->seg[seg_i], expr->type, v, data);
			}
		}
	}

	consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after symbol definition"));

	stmt_t* stmt = lt_amalloc(cx->arena, sizeof(stmt_t));
	if (sym->stype == SYM_LABEL)
		*stmt = STMT(STMT_LABEL);
	else
		*stmt = STMT(STMT_SYMDEF);
	stmt->sym = sym;
	stmt->expr = expr;
	stmt->type = type;

	if (!symtab_definable(cx->symtab, str))
		ferr("invalid redefinition of "A_BOLD"'%S'"A_RESET"", *tk, str);
	symtab_insert(cx->symtab, str, sym);
	return stmt;
}

stmt_t* parse_if(parse_ctx_t* cx) {
	tk_t* tk = consume(cx);
	stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
	*new = STMT(STMT_IF);
	new->expr = parse_expr(cx, NULL);

	if (!is_number(resolve_enum(new->expr->type)))
		ferr("result of "A_BOLD"'if'"A_RESET" condition must be a valid number", *tk);

	new->child = parse_compound(cx);

	tk = peek(cx, 0);
	if (tk->stype == TK_KW_ELSE) {
		consume(cx);
		new->child_2 = parse_compound(cx);
	}
	else if (tk->stype == TK_KW_ELIF)
		new->child_2 = parse_if(cx);

	return new;
}

#include <sys/stat.h>

typedef
struct file_id {
	u64 dev;
	u64 inode;
} file_id_t;

b8 get_fileid(lstr_t path_, file_id_t* id) {
	if (path_.len > LT_PATH_MAX)
		return 0;
	char path[LT_PATH_MAX];
	memcpy(path, path_.str, path_.len);
	path[path_.len] = 0;

	struct stat st;
	if (stat(path, &st) < 0)
		return 0;

	*id = (file_id_t){ st.st_dev, st.st_ino };
	return 1;
}

#include <lt/hashtab.h>
lt_hashtab_t file_tab;

file_id_t* hashtab_find_fid(lt_hashtab_t* htab, u32 hash, file_id_t* fid)
	LT_HASHTAB_FIND_IMPL(htab, hash, file_id_t* it, it->dev == fid->dev && it->inode == fid->inode)

#include <lt/strstream.h>

stmt_t* import_file(parse_ctx_t* cx, tk_t* path_tk) {
	file_id_t fid;

	lstr_t path = NLSTR();

	lstr_t esc_str = unescape_str(path_tk, (lt_alloc_t*)cx->arena);
	if (!esc_str.len)
		ferr("import string cannot be empty", *path_tk);
	if (esc_str.str[0] == '/') {
		if (get_fileid(esc_str, &fid)) {
			path = esc_str;
			goto success;
		}
		else
			goto fail;
	}

	for (usz i = 0; i < cx->include_dir_count; ++i) {
		lt_strstream_t s;
		LT_ASSERT(lt_strstream_create(&s, (lt_alloc_t*)cx->arena) == LT_SUCCESS);
		LT_ASSERT(lt_strstream_writels(&s, cx->include_dirs[i]) >= 0);
		LT_ASSERT(lt_strstream_writec(&s, '/') >= 0);
		LT_ASSERT(lt_strstream_writels(&s, esc_str) >= 0);
		path = s.str;
		if (get_fileid(path, &fid))
			goto success;
	}
fail:
	ferr("failed to open imported file", *path_tk);
success:
	u32 h = lt_hash(&fid, sizeof(fid));

	if (!hashtab_find_fid(&file_tab, h, &fid)) {
		file_id_t* fid_p = lt_amalloc(cx->arena, sizeof(file_id_t));
		*fid_p = fid;
		lt_hashtab_insert(&file_tab, h, fid_p, lt_libc_heap);

		lex_ctx_t* old_lex_cx = cx->lex;
		cx->lex = lex_file(cx->arena, path, path_tk);
		stmt_t* stmt = parse(cx);
		cx->lex = old_lex_cx;

		lt_amfree(cx->arena, esc_str.str);
		return stmt;
	}
	lt_amfree(cx->arena, esc_str.str);
	return NULL;
}

stmt_t* parse_stmt(parse_ctx_t* cx) {
	tk_t tk = *peek(cx, 0);
	switch (tk.stype) {
	case TK_SEMICOLON: consume(cx);
		return NULL;

	case TK_LEFT_BRACE:
		return parse_compound(cx);

	case TK_KW_IMPORT: consume(cx); {
		stmt_t* stmt = NULL;
 		stmt_t** it = &stmt;
		for (;;) {
			tk_t* path_tk = consume_type(cx, TK_STRING, CLSTR(", expected a path after 'import'"));
			*it = import_file(cx, path_tk);
			if (*it)
				it = &(*it)->child;

			if (peek(cx, 0)->stype != TK_COMMA) {
				consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after import statement"));
				return stmt;
			}
			consume(cx);
		}
	}

	case TK_KW_RETURN: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		type_t* ret_type = cx->curr_func_type->base;

		stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
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

	case TK_KW_IF: {
		if (!cx->curr_func_type)
			goto outside_func;
		return parse_if(cx);
	}

	case TK_KW_WHILE: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_WHILE);
		new->expr = parse_expr(cx, NULL);

		if (!is_number(resolve_enum(new->expr->type)))
			ferr("result of "A_BOLD"'while'"A_RESET" condition must be a valid number", tk);
		new->child = parse_compound(cx);
		return new;
	}

	case TK_KW_DO: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_DO);
		new->child = parse_compound(cx);

		consume_type(cx, TK_KW_WHILE, CLSTR(", expected "A_BOLD"'while'"A_RESET));
		new->expr = parse_expr(cx, NULL);

		if (!is_number(resolve_enum(new->expr->type)))
			ferr("result of "A_BOLD"'while'"A_RESET" condition must be a valid number", tk);
		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET));
		return new;
	}

	case TK_KW_FOR: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		PUSH_SCOPE();

		if (peek(cx, 0)->stype == TK_LEFT_BRACE) {
			stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
			*new = STMT(STMT_FOR);

			new->expr = NULL;
			new->sym = NULL;
			new->child = parse_compound(cx);
			return new;
		}

		tk_t ident_tk = *consume_type(cx, TK_IDENTIFIER, CLSTR(", expected identifier"));
		consume_type(cx, TK_COLON, CLSTR(", expected "A_BOLD"':'"A_RESET" after loop iterator"));

		sym_t* sym = lt_amalloc(cx->arena, sizeof(sym_t));
		*sym = SYM(SYM_VAR, ident_tk.str);
		sym->flags = SYMFL_ACCESSED;
		symtab_insert(cx->symtab, ident_tk.str, sym);

		tk_t* tk = peek(cx, 0);
		if (tk->stype == TK_DOUBLE_DOT) {
			sym->expr = lt_amalloc(cx->arena, sizeof(expr_t));
			*sym->expr = EXPR(EXPR_INTEGER, &i64_def, tk);
			sym->expr->int_val = 0;
			sym->type = sym->expr->type;
		}
		else {
			sym->expr = parse_expr(cx, NULL);
			sym->type = sym->expr->type;
			if (!is_int_any_sign(resolve_enum(sym->type)))
				ferr("starting point of for loop must be an integer", *tk);
		}

		consume_type(cx, TK_DOUBLE_DOT, CLSTR(", expected "A_BOLD"'..'"A_RESET));

		stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_FOR);

		tk = peek(cx, 0);
		new->expr = parse_expr(cx, NULL);
		if (!type_convert_implicit(cx, sym->type, &new->expr))
			ferr("cannot implicitly convert "A_BOLD"'%S'"A_RESET" to "A_BOLD"'%S'"A_RESET, *tk,
					type_to_reserved_str(cx->arena, new->expr->type), type_to_reserved_str(cx->arena, sym->type));

		new->sym = sym;
		new->child = parse_compound(cx);

		POP_SCOPE();

		return new;
	}

	case TK_KW_GOTO: consume(cx); {
		if (!cx->curr_func_type)
			goto outside_func;

		stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_GOTO);
		new->tk = consume_type(cx, TK_IDENTIFIER, CLSTR(", expected a label name"));
		return new;
	}

	case TK_KW_BREAK: {
		stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_BREAK);
		new->tk = consume(cx);
		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after break"));
		return new;
	}

	case TK_KW_CONTINUE: {
		stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));
		*new = STMT(STMT_CONTINUE);
		new->tk = consume(cx);
		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after continue"));
		return new;
	}

	case TK_KW_SWITCH: consume(cx); {
		tk_t* sw_tk = peek(cx, 0);

		stmt_t* sw = lt_amalloc(cx->arena, sizeof(stmt_t));
		*sw = STMT(STMT_SWITCH);
		sw->expr = parse_expr(cx, NULL);

		if (!is_number(resolve_enum(sw->expr->type)))
			ferr("switch type "A_BOLD"'%S'"A_RESET" is not a valid number", *sw_tk,
					type_to_reserved_str(cx->arena, sw->expr->type));

		stmt_t** case_it = &sw->child;

	parse_case:
		stmt_t* sw_case = lt_amalloc(cx->arena, sizeof(stmt_t));

		if (peek(cx, 0)->stype == TK_KW_DEFAULT) {
			*sw_case = STMT(STMT_DEFAULT);
			consume(cx);
			sw_case->child = parse_compound(cx);
		}
		else {
			*sw_case = STMT(STMT_CASE);
			consume_type(cx, TK_KW_CASE, CLSTR(", expected switch case"));
			expr_t** expr_it = &sw_case->expr;

		parse_case_expr:
			expr_t* expr = parse_expr(cx, sw->expr->type);

			*expr_it = expr;
			expr_it = &expr->next;
			if (peek(cx, 0)->stype == TK_COMMA) {
				consume(cx);
				goto parse_case_expr;
			}
			sw_case->child = parse_compound(cx);
		}

		*case_it = sw_case;
		case_it = &sw_case->next;
		if (peek(cx, 0)->stype == TK_COMMA) {
			consume(cx);
			goto parse_case;
		}
		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after switch statement"));
		return sw;
	}

	case TK_HASH: {
		// Parse
		tk_t* tk = consume(cx);
		type_t* old_func_type = cx->curr_func_type;
		cx->curr_func_type = &void_func_def;

		symtab_t* lbltab = symtab_create(cx->arena);

		PUSH_SCOPE();
		stmt_t* compound = parse_stmt(cx);
		POP_SCOPE();

		expr_t* lambda = lt_amalloc(cx->arena, sizeof(expr_t));
		*lambda = EXPR(EXPR_LAMBDA, &void_func_def, tk);
		lambda->stmt = compound;
		lambda->label_symtab = lbltab;

		cx->curr_func_type = old_func_type;

		ival_t seg_v = gen_const_expr(cx->gen_cx, lambda);
		LT_ASSERT(seg_v.stype == IVAL_SEG);
		usz seg_i = seg_v.uint_val;

		usz func = jit_compile_func(cx->gen_cx, seg_i);
		((void(*)(void))func)();
		return NULL;
	}

	case TK_KW_STRUCT: case TK_KW_ENUM:
	default: {
		stmt_t* new = lt_amalloc(cx->arena, sizeof(stmt_t));

		if (tk.stype == TK_IDENTIFIER && peek(cx, 1)->stype == TK_COLON) {
			consume(cx);
			return parse_symdef(cx, tk.str);
		}

		if (!cx->curr_func_type) {
			goto outside_func;
		}
		*new = STMT(STMT_EXPR);
		new->expr = parse_expr(cx, NULL);

		consume_type(cx, TK_SEMICOLON, CLSTR(", expected "A_BOLD"';'"A_RESET" after expression"));
		return new;
	}
	}

outside_func:
	ferr(A_BOLD"'%S'"A_RESET" must be inside a function body", tk, tk.str);
}

