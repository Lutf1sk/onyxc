#ifndef PARSE_H
#define PARSE_H 1

#include <lt/lt.h>
#include <lt/mem.h>

#include "fwd.h"

typedef struct parse_ctx {
	tk_t* data;
	usz count;
	usz it;

	char* path;

	symtab_t* symtab;

	lt_arena_t* arena;
} parse_ctx_t;

typedef
enum operator_assoc {
	OP_ASSOC_LEFT,
	OP_ASSOC_RIGHT,
} operator_assoc_t;

typedef
struct operator {
	u32 tk;
	int precedence;
	u32 expr;
	operator_assoc_t associate;
} operator_t;

extern operator_t operators[];
extern operator_t sfx_operators[];
extern operator_t pfx_operators[];

typedef stmt_t* (*parse_pfn)(parse_ctx_t* cx);

// parse_stmt.c
stmt_t* parse_func_body(parse_ctx_t* cx);
stmt_t* parse_compound(parse_ctx_t* cx);
stmt_t* parse_stmt(parse_ctx_t* cx);
extern parse_pfn parse;

// parse_expr.c
expr_t* parse_expr_primary(parse_ctx_t* cx, type_t* type);
expr_t* parse_expr_unary(parse_ctx_t* cx, type_t* type);
expr_t* parse_expr_binary(parse_ctx_t* cx, type_t* type, int precedence);
expr_t* parse_expr(parse_ctx_t* cx, type_t* type);

// parse_type.c
type_t* parse_type(parse_ctx_t* cx);

#endif
