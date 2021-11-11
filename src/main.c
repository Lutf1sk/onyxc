#include <lt/io.h>
#include <lt/mem.h>
#include <lt/str.h>

#include "expr_ast.h"
#include "stmt_ast.h"
#include "lex.h"
#include "type.h"
#include "parse.h"
#include "symtab.h"

#include "interm.h"
#include "gen.h"
#include "segment.h"

#include "exec.h"

void type_print(lt_arena_t* arena, type_t* type) {
	char* str_data = lt_arena_reserve(arena, 0);
	lstr_t str = LSTR(str_data, type_to_str(str_data, type));
	lt_printls(str);
}

void stmt_print_recursive(lt_arena_t* arena, stmt_t* st, int indent);

void print_indent(int indent) {
	for (int i = 0; i + 1 < indent; ++i)
		lt_printls(CLSTR("| "));
	if (indent)
		lt_printls(CLSTR("|-"));
}

void expr_print_recursive(lt_arena_t* arena, expr_t* ex, int indent) {
	expr_t* it = ex;
	while (it) {
		print_indent(indent);
		lt_printf("%S: ", expr_type_str(it->stype));
		if (it->type) {
			type_print(arena, it->type);
			lt_printc(' ');
		}
		if (it->stype == EXPR_SYM)
			lt_printf("%S ", it->sym->name);
		if (it->stype == EXPR_INTEGER)
			lt_printf("%iq ", it->int_val);

		lt_printc('\n');
		if (it->sym && it->stype == EXPR_LAMBDA)
			stmt_print_recursive(arena, it->stmt, indent + 1);
		if (it->child_1)
			expr_print_recursive(arena, it->child_1, indent + 1);
		if (it->child_2)
			expr_print_recursive(arena, it->child_2, indent + 1);
		it = it->next;
	}
}

void stmt_print_recursive(lt_arena_t* arena, stmt_t* st, int indent) {
	stmt_t* it = st;
	while (it) {
		print_indent(indent);

		lt_printf("%S: ", stmt_type_str(it->stype));
		if (it->type) {
			type_print(arena, it->type);
			lt_printc(' ');
		}
		if (it->stype == STMT_DEF || it->stype == STMT_LET)
			lt_printf("%S ", it->sym->name);
		lt_printc('\n');
		if (it->expr)
			expr_print_recursive(arena, it->expr, indent + 1);
		if (it->child)
			stmt_print_recursive(arena, it->child, indent + 1);
		if (it->child_2)
			stmt_print_recursive(arena, it->child_2, indent + 1);
		it = it->next;
	}
}

void stmt_print(lt_arena_t* arena, stmt_t* st) {
	stmt_print_recursive(arena, st, 0);
}

#define PRIMITIVE_INITIALIZER(T) CLSTR(#T), { SYM_TYPE, 0, CLSTR(#T), &T##_def, NULL }

void print_ival(ival_t ival) {
	switch (ival.stype) {
	case IVAL_REG: lt_printf("r%iq ", ival.reg); break;
	case IVAL_IMM: lt_printf("%iq ", ival.uint_val); break;
	case IVAL_DSO: lt_printf("ds'%iq+%id*%ib ", ival.dso, ival.index, ival.scale); break;
	case IVAL_CSO: lt_printf("cs'%iq+%id*%ib ", ival.cso, ival.index, ival.scale); break;
	case IVAL_SFO: lt_printf("sf'%iq+%id*%ib ", ival.sfo, ival.index, ival.scale); break;

	case IVAL_REG | IVAL_REF: lt_printf("[r%iq+%id*%ib] ", ival.reg, ival.index, ival.scale); break;
	case IVAL_IMM | IVAL_REF: lt_printf("[%iq+%id*%ib] ", ival.uint_val, ival.index, ival.scale); break;
	case IVAL_DSO | IVAL_REF: lt_printf("[ds'%iq+%id*%ib] ", ival.dso, ival.index, ival.scale); break;
	case IVAL_CSO | IVAL_REF: lt_printf("[cs'%iq+%id*%ib] ", ival.cso, ival.index, ival.scale); break;
	case IVAL_SFO | IVAL_REF: lt_printf("[sf'%iq+%id*%ib] ", ival.sfo, ival.index, ival.scale); break;
	}
}

int main(int argc, char** argv) {
	char** in_files = malloc(argc * sizeof(char*));
	if (!in_files)
		lt_ferr(CLSTR("memory allocation failed\n"));
	usz in_file_count = 0;

	b8 run_mode = 0;

	for (usz i = 1; i < argc; ++i) {
		if (argv[i][0] == '-') {
			char* flag_start = &argv[i][1];
			usz flag_len = strlen(flag_start);
			if (lt_lstr_eq(LSTR(flag_start, flag_len), CLSTR("run")))
				run_mode = 1;
		}
		else
			in_files[in_file_count++] = argv[i];
	}

	if (!in_file_count)
		lt_ferrf("No input file provided\n");

	for (isz file_index = 0; file_index < in_file_count; ++file_index) {
		char* in_path = in_files[file_index];

		// Read source file
		lt_arena_t* lex_arena = lt_arena_alloc(LT_MB(128));
		lt_file_t* fp = lt_file_open(lex_arena, in_path, LT_FILE_R, 0);
		if (!fp)
			lt_ferrf("Failed to open '%s'\n", in_path);

		usz size = lt_file_size(fp);
		char* data = lt_arena_reserve(lex_arena, size + 1);
		if (lt_file_read(fp, data, size) != size)
			lt_ferrf("Failed to read from '%s'", argv[1]);
		data[size] = 0;
		lt_file_close(fp);

		// Lex data
		lex_ctx_t lex_cx;
		lex_cx.path = in_path;
		lex_cx.data = LSTR(data, size);

		usz tk_count = lex(&lex_cx, lt_arena_reserve(lex_arena, 0));
		tk_t* tk_data = lt_arena_reserve(lex_arena, tk_count * sizeof(tk_t));

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
		};

		for (usz i = 0; i < sizeof(primitives) / sizeof(*primitives); ++i)
			symtab_insert(&symtab, primitives[i].name, &primitives[i].sym);

		// Parse token array
		lt_arena_t* parse_arena = lt_arena_alloc(LT_MB(128));

		parse_ctx_t parse_cx;
		memset(&parse_cx, 0, sizeof(parse_cx));
		parse_cx.data = tk_data;
		parse_cx.count = tk_count;
		parse_cx.path = in_path;
		parse_cx.arena = parse_arena;
		parse_cx.symtab = &symtab;
		parse_cx.lex = &lex_cx;
		stmt_t* root = parse(&parse_cx);

		// Print AST
	 	if (!run_mode)
			stmt_print(lex_arena, root);

		// Generate intermediate code
		gen_ctx_t gen_cx;
		gen_cx.curr_func = -1;
		gen_cx.code_seg = NULL;
		gen_cx.code_seg_count = 0;
		gen_cx.data_seg = NULL;
		gen_cx.data_seg_count = 0;
		gen_cx.arena = parse_arena;

		icode_gen(&gen_cx, root);

		if (!run_mode) {
			// Print intermediate code
			for (usz i = 0; i < gen_cx.code_seg_count; ++i) {
				icode_t* icode = gen_cx.code_seg[i].data;
				usz icode_count = gen_cx.code_seg[i].size;

				lt_printf("CS %uq:\n", i);
				for (usz i = 0; i < icode_count; ++i) {
					icode_t ic = icode[i];

					lt_printc('\t');
					lt_printf("%S%S%S", icode_size_str(ic.arg1), icode_size_str(ic.arg2), icode_size_str(ic.arg3));

					lt_printf("\t%S\t", icode_type_str(ic.op));
					print_ival(ic.arg1);
					print_ival(ic.arg2);
					print_ival(ic.arg3);
					lt_printc('\n');
				}
			}

			// Print data segments
			for (usz i = 0; i < gen_cx.data_seg_count; ++i) {
				seg_ent_t* seg = &gen_cx.data_seg[i];
				lt_printf("DS %uq %S: %uq bytes\n", i, seg->name, seg->size);
			}
		}

		// Execute intermediate code
		u64* stack = lt_arena_reserve(parse_arena, LT_MB(1));
		u64 code;

		sym_t* main = symtab_find(&symtab, CLSTR("main"));
		if (!main)
			lt_ferr(CLSTR("program has no entry point\n"));
		if (main->type->stype != TP_FUNC || !(main->flags & SYMFL_CONST) || main->ival.stype != IVAL_CSO)
			lt_ferr(CLSTR("'main' symbol must be a function\n"));

		exec_ctx_t exec_cx;
		exec_cx.sp = (u8*)stack;
		exec_cx.ip = gen_cx.code_seg[main->ival.cso].data;
		exec_cx.cs = gen_cx.code_seg;
		exec_cx.ds = gen_cx.data_seg;
		exec_cx.ret_ptr = (u8*)&code;

		icode_exec(&exec_cx);
		if (!run_mode)
			lt_printf("program exited with code %uq\n", code);
		else
			return code;

		lt_arena_free(parse_arena);
		lt_arena_free(lex_arena);
	}

	free(in_files);
	return 0;
}

