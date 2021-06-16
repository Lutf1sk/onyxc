#ifndef PARSE_H
#define PARSE_H

#include "common.h"
#include "fwd.h"

typedef
struct PACKED Register {
    b8 allocated : 1;
} Register;

typedef
struct ParseCtx {
	const char* file_path;
	char* char_data;

	const Token* tk_data;
	usz tk_len;
	usz tk_it;

	IntermediateFunc* funcs;
	usz func_count;
	usz func_alloc_count;
	isz curr_func;

	usz regs_used;

	Symbols* syms;

	Types* types;
} ParseCtx;


Expression parse_expr(ParseCtx* cx);
usz gen_expr(ParseCtx* cx, Expression* expr);

void parse_compound(ParseCtx* cx);
void parse_stmt(ParseCtx* cx);

b8 parse_type(ParseCtx* cx, TypeHandle* ret_hnd);

void parse(ParseCtx* cx);

#endif
