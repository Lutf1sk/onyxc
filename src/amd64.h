#ifndef AMD64_H
#define AMD64_H 1

#include "fwd.h"

#include <lt/fwd.h>
#include <lt/lt.h>

typedef
struct amd64_ctx {
	lt_arena_t* arena;

	seg_ent_t* ir_cs;
	seg_ent_t* ir_ds;
	usz ir_cs_count, ir_ds_count;

	seg_ent_t* cs;
	seg_ent_t* ds;

	u8 reg_map[256];
} amd64_ctx_t;

void amd64_gen(amd64_ctx_t* cx);

#endif
