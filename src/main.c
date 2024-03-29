#include <lt/io.h>
#include <lt/mem.h>
#include <lt/str.h>
#include <lt/arg.h>

#include "expr_ast.h"
#include "stmt_ast.h"
#include "lex.h"
#include "type.h"
#include "parse.h"
#include "symtab.h"

#include "interm.h"
#include "gen.h"
#include "segment.h"

#include "target.h"
#include "amd64/amd64.h"

#include "textattrib.h"

#include <lt/text.h>

void type_print(lt_arena_t* arena, type_t* type) {
	char* str_data = lt_amalloc(arena, 0);
	lstr_t str = LSTR(str_data, type_to_str(str_data, type));
	lt_printf("%S", str);
}

void stmt_print_recursive(lt_arena_t* arena, stmt_t* st, char* indent, b8 last);

void expr_print_recursive(lt_arena_t* arena, expr_t* ex, char* indent, b8 last) {
	usz indent_len = strlen(indent);

	expr_t* it = ex;
	while (it) {
		lt_printf(!last ? "%s\u251C" : "%s\u2514", indent);
		indent[indent_len + lt_sprintf(&indent[indent_len], !last ? "\u2502" : " ")] = 0;

		lt_printf("%S ", expr_type_str(it->stype));
		if (it->type) {
			type_print(arena, it->type);
			lt_printf(" ");
		}
		if (it->stype == EXPR_SYM)
			lt_printf("%S ", it->sym->name);
		if (it->stype == EXPR_INTEGER)
			lt_printf("%iq ", it->int_val);
		lt_printf("\n");

		if (it->sym && it->stype == EXPR_LAMBDA) {
			stmt_print_recursive(arena, it->stmt, indent, it->child_1 == NULL && it->child_2 == NULL);
		}
		if (it->child_1) {
			expr_print_recursive(arena, it->child_1, indent, it->child_2 == NULL && it->child_1->next == NULL);
		}
		if (it->child_2) {
			expr_print_recursive(arena, it->child_2, indent, it->child_2->next == NULL);
		}

		indent[indent_len] = 0;
		it = it->next;
		if (it)
			last = it->next == NULL;
	}
}

void stmt_print_recursive(lt_arena_t* arena, stmt_t* st, char* indent, b8 last) {
	usz indent_len = strlen(indent);

	stmt_t* it = st;
	while (it) {
		lt_printf(!last ? "%s\u251C" : "%s\u2514", indent);
		indent[indent_len + lt_sprintf(&indent[indent_len], !last ? "\u2502" : " ")] = 0;

		lt_printf("%S ", stmt_type_str(it->stype));
		if (it->stype == STMT_SYMDEF)
			lt_printf("%S ", it->sym->name);
		if (it->type) {
			type_print(arena, it->type);
			lt_printf(" ");
		}
		lt_printf("\n");

		if (it->expr) {
			expr_print_recursive(arena, it->expr, indent, it->child == NULL && it->child_2 == NULL && it->expr->next == NULL);
		}
		if (it->child) {
			stmt_print_recursive(arena, it->child, indent, it->child_2 == NULL && it->child->next == NULL);
		}
		if (it->child_2) {
			stmt_print_recursive(arena, it->child_2, indent, it->child_2->next == NULL);
		}

		indent[indent_len] = 0;
		it = it->next;
		if (it)
			last = it->next == NULL;
	}
}

void stmt_print(lt_arena_t* arena, stmt_t* st) {
	char indent_buf[512] = "";
	lt_printf("ROOT:\n");
	stmt_print_recursive(arena, st, indent_buf, st->next == NULL);
}

#define PRIMITIVE_INITIALIZER(T) CLSTR(#T), { SYM_TYPE, 0, CLSTR(#T), &T##_def, NULL }

#if defined(LT_UNIX)
#	define STDPATH CLSTR("/usr/lib/onyxc")
#elif defined(LT_WINDOWS)
#	define STDPATH CLSTR("??")
#endif

int main(int argc, char** argv) {
	LT_DEBUG_INIT();

	char** in_files = malloc(argc * sizeof(char*));
	if (!in_files)
		lt_ferr(CLSTR("memory allocation failed\n"));
	usz in_file_count = 0;

	lstr_t out_path = CLSTR("out");

	b8 print_tokens = 0;
	b8 print_ast = 0;
	b8 print_icode = 0;
	b8 print_mcode = 0;

	lt_arg_iterator_t arg_it = lt_arg_iterator_create(argc, argv);

#define INCLUDE_DIR_MAX 128
	lstr_t include_dirs[INCLUDE_DIR_MAX] = { CLSTR("."), STDPATH };
	usz include_dir_count = 2;

	while (lt_arg_next(&arg_it)) {
		if (lt_arg_flag(&arg_it, 'h', CLSTR("help"))) {
			lt_printf(
				"usage: onyxc [OPTIONS] FILE...\n"
				"options:\n"
				"  -h, --help          Display this information.\n"
				"  -o, --out=OUT       Store compiled program in OUT\n"
				"  -I  --include=PATH  Add include directory PATH\n"
				"  --print-tokens      Print lexer output\n"
				"  --print-ast         Print AST\n"
				"  --print-icode       Print generated intermediate code\n"
				"  --print-mcode       Print generated assembly\n"
			);
			return 0;
		}
		if (lt_arg_flag(&arg_it, 0, CLSTR("print-tokens"))) {
			print_tokens = 1;
			continue;
		}
		if (lt_arg_flag(&arg_it, 0, CLSTR("print-ast"))) {
			print_ast = 1;
			continue;
		}
		if (lt_arg_flag(&arg_it, 0, CLSTR("print-icode"))) {
			print_icode = 1;
			continue;
		}
		if (lt_arg_flag(&arg_it, 0, CLSTR("print-mcode"))) {
			print_mcode = 1;
			continue;
		}
		if (lt_arg_str(&arg_it, 'o', CLSTR("out"), &out_path.str)) {
			out_path.len = strlen(out_path.str);
			continue;
		}
		char* dir;
		if (lt_arg_str(&arg_it, 'I', CLSTR("include"), &dir)) {
			LT_ASSERT(include_dir_count < INCLUDE_DIR_MAX);
			include_dirs[include_dir_count++] = LSTR(dir, strlen(dir));
			continue;
		}

		in_files[in_file_count++] = *arg_it.it;
	}

 	u32 target = TRG_AMD64;
 	u32 format = FMT_ELF64;

	if (!in_file_count)
		lt_ferrf("No input file provided\n");

	for (isz file_index = 0; file_index < in_file_count; ++file_index) {
		lstr_t in_path = LSTR(in_files[file_index], strlen(in_files[file_index]));

		lt_arena_t* arena = lt_amcreate(NULL, LT_GB(1), 0);
		lex_ctx_t* lex_cx = lex_file(arena, in_path, NULL);

		if (print_tokens) {
			for (usz i = 0; i < lex_cx->count; ++i) {
				tk_t* tk = &lex_cx->tk_data[i];
				lt_printf("[%S] '%S'\n", tk_type_str(tk->stype), tk->str);
			}
		}

		// Create symtab and add primitives
		symtab_t symtab;
		memset(&symtab, 0, sizeof(symtab_t));

		struct {
			lstr_t name;
			sym_t sym;
		} primitives[] = {
			{ PRIMITIVE_INITIALIZER(void) },

			{ PRIMITIVE_INITIALIZER(u8) },
			{ PRIMITIVE_INITIALIZER(u16) },
			{ PRIMITIVE_INITIALIZER(u32) },
			{ PRIMITIVE_INITIALIZER(u64) },

			{ PRIMITIVE_INITIALIZER(i8) },
			{ PRIMITIVE_INITIALIZER(i16) },
			{ PRIMITIVE_INITIALIZER(i32) },
			{ PRIMITIVE_INITIALIZER(i64) },

			{ PRIMITIVE_INITIALIZER(b8) },

			{ PRIMITIVE_INITIALIZER(f32) },
			{ PRIMITIVE_INITIALIZER(f64) },

			{ CLSTR("isz"), { SYM_TYPE, 0, CLSTR("isz"), &i64_def, NULL } },
			{ CLSTR("usz"), { SYM_TYPE, 0, CLSTR("usz"), &u64_def, NULL } },
		};

		for (usz i = 0; i < sizeof(primitives) / sizeof(*primitives); ++i)
			symtab_insert(&symtab, primitives[i].name, &primitives[i].sym);

		segtab_t segtab = { 0, NULL };

		// Parse token array
		gen_ctx_t gen_cx;
		gen_cx.lex_cx = lex_cx;
		gen_cx.curr_func = -1;
		gen_cx.segtab = &segtab;
		gen_cx.arena = arena;

		parse_ctx_t parse_cx;
		memset(&parse_cx, 0, sizeof(parse_cx));
		parse_cx.arena = arena;
		parse_cx.symtab = &symtab;
		parse_cx.label_symtab = NULL;
		parse_cx.lex = lex_cx;
		parse_cx.gen_cx = &gen_cx;
		parse_cx.include_dirs = include_dirs;
		parse_cx.include_dir_count = include_dir_count;
		stmt_t* root = parse(&parse_cx);

		if (print_ast) {
			stmt_print(arena, root);
		}

		// Generate intermediate code
		icode_gen(&gen_cx, root);

		if (print_icode) {
			for (usz i = 0; i < segtab.count; ++i)
				print_segment(&segtab, i);
		}

		switch (target) {
		case TRG_AMD64: {
			amd64_ctx_t x64;
			x64.arena = arena;
			x64.segtab = &segtab;
			x64.reg_map = lt_amalloc(arena, sizeof(amd64_ireg_t) * 2048); // !!
			x64.reg_lifetimes = lt_amalloc(arena, sizeof(usz) * 2048); // !!

			amd64_gen(&x64);

			if (print_mcode) {
				for (usz i = 0; i < segtab.count; ++i) {
					if (segtab.seg[i].stype != SEG_CODE)
						continue;

					amd64_print_segment(&x64, i);
				}
			}

			if (format == FMT_ELF64)
				amd64_write_elf64(&x64, out_path);
		}	break;

		case TRG_X86: {

		}	break;
		}

		lt_amdestroy(arena);
	}

	free(in_files);
	return 0;
}

