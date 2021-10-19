#ifndef GEN_H
#define GEN_H 1

#include "fwd.h"
#include <lt/fwd.h>

typedef
struct gen_ctx {
	isz curr_func;

	usz code_seg_count;
	seg_ent_t* code_seg;

	usz data_seg_count;
	seg_ent_t* data_seg;

	lt_arena_t* arena;
} gen_ctx_t;

ival_t icode_gen_expr(gen_ctx_t* cx, expr_t* expr);
void icode_gen_stmt(gen_ctx_t* cx, stmt_t* stmt);

void icode_gen(gen_ctx_t* cx, stmt_t* root);

#endif
