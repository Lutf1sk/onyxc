#include <lt/io.h>

#include "jit.h"
#include "segment.h"
#include "gen.h"
#include "amd64/amd64.h"

usz jit_compile_func(gen_ctx_t* gen_cx, usz seg_i) {
	amd64_ctx_t x64;
	x64.arena = gen_cx->arena;
	x64.segtab = gen_cx->segtab;
	x64.reg_map = lt_amalloc(gen_cx->arena, sizeof(amd64_ireg_t) * 2048); // !!
	x64.reg_lifetimes = lt_amalloc(gen_cx->arena, sizeof(usz) * 2048); // !!

	return (usz)amd64_jit_link_segment(&x64, seg_i);
}

