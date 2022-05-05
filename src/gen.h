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

usz new_code_seg(gen_ctx_t* cx, type_t* type);
usz new_data_seg(gen_ctx_t* cx, seg_ent_t new_ent);

u8* ival_write_comp(gen_ctx_t* cx, type_t* type, ival_t v, u8* out);

ival_t gen_const_expr(gen_ctx_t* cx, expr_t* expr);

ival_t icode_gen_expr(gen_ctx_t* cx, expr_t* expr);
void icode_gen_stmt(gen_ctx_t* cx, stmt_t* stmt);

void icode_gen(gen_ctx_t* cx, stmt_t* root);

#endif
