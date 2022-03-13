#ifndef GEN_H
#define GEN_H 1

#include "fwd.h"
#include <lt/fwd.h>

typedef
struct gen_ctx {
	isz curr_func;
	lex_ctx_t* lex_cx;

	usz seg_count;
	seg_ent_t* seg;

	lt_arena_t* arena;
} gen_ctx_t;

ival_t gen_const_expr(gen_ctx_t* cx, expr_t* expr);

ival_t icode_gen_expr(gen_ctx_t* cx, expr_t* expr);
void icode_gen_stmt(gen_ctx_t* cx, stmt_t* stmt);

void icode_gen(gen_ctx_t* cx, stmt_t* root);

#endif
