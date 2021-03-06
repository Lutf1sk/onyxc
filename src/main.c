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

#include "exec.h"

#include "target.h"
#include "amd64/amd64.h"

#include "textattrib.h"

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

int main(int argc, char** argv) {
	char** in_files = malloc(argc * sizeof(char*));
	if (!in_files)
		lt_ferr(CLSTR("memory allocation failed\n"));
	usz in_file_count = 0;

	lstr_t out_path = CLSTR("a.out");
	b8 run_mode = 0;
	lt_arg_bool(CLSTR("run"), &run_mode);
	lt_arg_bool(CLSTR("r"), &run_mode);

	lt_arg_str(CLSTR("out"), &out_path);
	lt_arg_str(CLSTR("o"), &out_path);

	lt_arg_parse(argc, argv);

	for (usz i = 1; i < argc; ++i) {
		if (argv[i][0] != '-')
			in_files[in_file_count++] = argv[i];
	}

 	u32 target = TRG_AMD64;
 	u32 format = FMT_ELF64;

	if (!in_file_count)
		lt_ferrf("No input file provided\n");

	for (isz file_index = 0; file_index < in_file_count; ++file_index) {
		char* in_path = in_files[file_index];

		lt_arena_t* arena = lt_arena_alloc(LT_MB(128));
		lex_ctx_t* lex_cx = lex_file(arena, in_path, NULL);

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

		// Parse token array
		gen_ctx_t gen_cx;
		gen_cx.lex_cx = lex_cx;
		gen_cx.curr_func = -1;
		gen_cx.seg = NULL;
		gen_cx.seg_count = 0;
		gen_cx.arena = arena;

		parse_ctx_t parse_cx;
		memset(&parse_cx, 0, sizeof(parse_cx));
		parse_cx.arena = arena;
		parse_cx.symtab = &symtab;
		parse_cx.label_symtab = NULL;
		parse_cx.lex = lex_cx;
		parse_cx.gen_cx = &gen_cx;
		stmt_t* root = parse(&parse_cx);

#if 0
		// Print AST
		stmt_print(arena, root);
#endif

		// Generate intermediate code
		icode_gen(&gen_cx, root);

		if (run_mode) {
			// Execute intermediate code
			u64* stack = lt_arena_reserve(arena, LT_MB(1));
			u64 code = 0;

			sym_t* main = symtab_find(&symtab, CLSTR("main"));
			if (!main)
				lt_ferr(CLSTR("program has no entry point\n"));
			if (main->type->stype != TP_FUNC || !(main->flags & SYMFL_CONST) || main->val.stype != IVAL_SEG)
				lt_ferr(CLSTR("'main' symbol must be a function\n"));

			exec_ctx_t exec_cx;
			exec_cx.sp = (u8*)stack;
			exec_cx.bp = (u8*)stack;
			exec_cx.ip = gen_cx.seg[main->val.uint_val].data;
			exec_cx.seg = gen_cx.seg;
			exec_cx.ret_ptr = (u8*)&code;
			exec_cx.reg_offs = 0;

			icode_exec(&exec_cx);
			return code;
		}

#if 0
		// Print segments
		for (usz i = 0; i < gen_cx.seg_count; ++i) {
			seg_ent_t* seg = &gen_cx.seg[i];
			if (seg->stype == SEG_DATA) {
				lt_printf("DS %uq '%S': %uq bytes\n", i, seg->name, seg->size);
				continue;
			}

			icode_t* icode = seg->data;
			usz icode_count = seg->size;

			lt_printf("CS %uq '%S':\n", i, seg->name);
			for (usz i = 0; i < icode_count; ++i) {
				icode_t ic = icode[i];

				lt_printf("%uz\t%S\t%S\t", i, icode_size_str(ic.size), icode_op_str(ic.op));

				if (ic.dst)
					lt_printf(A_BOLD"r%iq "A_RESET, ic.dst);
				switch (ic.op) {
				case IR_INT: lt_printf("0x%hq\n", ic.uint_val); break;
				case IR_FLOAT: lt_printf("FLOAT\n"); break;
				case IR_SRESV: lt_printf("%ud %ud\n", ic.regs[0], ic.regs[1]); break;
				case IR_GETLBL: lt_printf("%iq\n", ic.int_val); break;
				case IR_LBL: lt_printf("%ud\n", ic.uint_val); break;
				case IR_SEG: lt_printf("%ud\n", ic.regs[0]); break;
				case IR_CALL: lt_printf("r%ud %ud\n", ic.regs[0], ic.regs[1]); break;
				case IR_SYSCALL: lt_printf("%ud\n", ic.regs[0]); break;
				case IR_SETARG: lt_printf("%ud\n", ic.regs[0]); break;
				case IR_GETARG: lt_printf("%ud\n", ic.regs[0]); break;
				default:
					for (usz i = 0; i < 2; ++i)
						if (ic.regs[i])
							lt_printf("r%ud ", ic.regs[i]);
					lt_printc('\n');
					break;
				}
			}
		}
#endif

		switch (target) {
		case TRG_AMD64: {
			amd64_ctx_t x64;
			x64.arena = arena;
			x64.seg = gen_cx.seg;
			x64.seg_count = gen_cx.seg_count;

			amd64_gen(&x64);

#if 1
			// Print machine code
			for (usz i = 0; i < x64.seg_count; ++i) {
				if (x64.seg[i].stype != SEG_MCODE)
					continue;

				lt_printf("\nM-CS %uq '%S':\n", i, x64.seg[i].name);
				amd64_print_seg(&x64, i);
			}
#endif

			if (format == FMT_ELF64)
				amd64_write_elf64(&x64, out_path.str);
		}	break;

		case TRG_X86: {

		}	break;
		}

		lt_arena_free(arena);
	}

	free(in_files);
	return 0;
}

