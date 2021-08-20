// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "filehl.h"
#include "scan.h"
#include "tk.h"
#include "err.h"
#include "ast.h"
#include "parse.h"
#include "type.h"
#include "sym.h"
#include "intermediate.h"
#include "parse_helpers.h"

int main() {
	const char* in_path = "test.nyx";

	FILE* fp = hl_fopen_r(in_path);
	if (!fp)
		err("Failed to open file");

	usz chars = hl_fsize(fp);
	char* char_data = malloc(chars + 1);
	assert(char_data);
	assert(hl_fread(fp, char_data, chars) == chars);
	fclose(fp);
	char_data[chars] = 0; // Null-terminate string

	ScanCtx scx = {};
	scx.file_path = in_path;
	scx.line_index = 0;
	scx.char_data = char_data;
	scx.char_count = chars;
	scx.char_it = 0;
	scx.token_data = NULL;
	scx.token_count = 0;
	scx.token_avail_count = 0;

	scan(&scx);

	printf("Got %zu tokens:\n", scx.token_count);
	for (usz i = 0; i < scx.token_count; ++i) {
		Token tk = scx.token_data[i];
		printf("%15s %.*s\n",
			   tk_type_str(tk.type), (int)tk.len, &char_data[tk.start]);
	}

	Symbols sym_tab = make_sym_tab();

	Types type_tab = make_type_tab();

	typedef
	struct Primitive {
		LenStr name;
		TypeSType type;
		TypeHandle* out_hnd;
	} Primitive;

	Primitive primitives[] = {
		{ LSTR("u8",  2), TP_U8,  &u8_hnd  },
		{ LSTR("u16", 3), TP_U16, &u16_hnd },
		{ LSTR("u32", 3), TP_U32, &u32_hnd },
		{ LSTR("u64", 3), TP_U64, &u64_hnd },

		{ LSTR("i8",  2), TP_I8,  &i8_hnd  },
		{ LSTR("i16", 3), TP_I16, &i16_hnd },
		{ LSTR("i32", 3), TP_I32, &i32_hnd },
		{ LSTR("i64", 3), TP_I64, &i64_hnd },

		{ LSTR("f32", 3), TP_F32, &f32_hnd },
		{ LSTR("f64", 3), TP_F64, &f64_hnd },
	};

	const usz primitive_count = sizeof(primitives) / sizeof(*primitives);

	for (usz i = 0; i < primitive_count; ++i) {
		usz offs = add_primitive(&type_tab, primitives[i].name, primitives[i].type);
		*primitives[i].out_hnd = make_type_handle(&type_tab, offs);
		add_symbol(&sym_tab, SM_TYPE, primitives[i].name, make_type_handle(&type_tab, offs));
	}

	ParseCtx pcx = {};
	pcx.file_path = in_path;
	pcx.char_data = char_data;
	pcx.tk_data = scx.token_data;
	pcx.tk_len = scx.token_count;
	pcx.tk_it = 0;
	pcx.funcs = NULL;
	pcx.func_count = 0;
	pcx.func_alloc_count = 0;
	pcx.curr_func = -1;
	pcx.regs_used = 0;
	pcx.types = &type_tab;
	pcx.syms = &sym_tab;

	parse(&pcx);

	printf("\nGot %zu global symbols:\n", sym_tab.sym_count);
	for (usz i = 0; i < sym_tab.sym_count; ++i) {
		LenStr name = sym_tab.names[i];
		printf("\t%.*s\n", (int)name.len, name.str);
	}

	for (usz i = 0; i < pcx.func_count; ++i) {
		printf("\nFunction %zu:\n", i);
		IntermediateFunc func = pcx.funcs[i];
		for (usz j = 0; j < func.instr_count; ++j) {
			putchar('\t');
			print_instr(func.instrs[j]);
			putchar('\n');
		}
		free_func(&func);
	}

	if (pcx.funcs)
		free(pcx.funcs);

	free_type_tab(&type_tab);
	free_sym_tab(&sym_tab);
	free(scx.token_data);
	free(char_data);
	return 0;
}
