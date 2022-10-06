#ifndef GEN_H
#define GEN_H 1

#include "fwd.h"
#include <lt/fwd.h>

#include "interm.h"

typedef
struct gen_ctx {
	isz curr_func;
	lex_ctx_t* lex_cx;

	segtab_t* segtab;

	lt_arena_t* arena;

	usz cont_lbl;
	usz break_lbl;
} gen_ctx_t;

u8* ival_write_comp(gen_ctx_t* cx, seg_ent_t* seg, type_t* type, ival_t v, u8* out);

ival_t gen_const_expr(gen_ctx_t* cx, expr_t* expr);

ival_t icode_gen_expr(gen_ctx_t* cx, expr_t* expr);
void icode_gen_stmt(gen_ctx_t* cx, stmt_t* stmt);

void icode_gen(gen_ctx_t* cx, stmt_t* root);

#endif
