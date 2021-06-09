#ifndef PARSE_H
#define PARSE_H

#include "common.h"
#include "fwd.h"

typedef
struct ParseCtx {
	const char* file_path;
	char* char_data;

	const Token* tk_data;
	usz tk_len;
	usz tk_it;

	Symbols* syms;

	usz type_count;

	Types* types;
} ParseCtx;

Expression parse_expr(ParseCtx* cx);

void parse_compound(ParseCtx* cx);
void parse_stmt(ParseCtx* cx);

b8 parse_type(ParseCtx* cx, TypeHandle* ret_hnd);

void parse(ParseCtx* cx);

#endif
