#ifndef AMD64_H
#define AMD64_H 1

#include "../fwd.h"

#include <lt/fwd.h>
#include <lt/lt.h>

// amd64.c

#define AMD64_REG_COUNT 16
#define AMD64_RMAP_PRESENT 1
#define AMD64_RMAP_REF 2

#define REGM_REG 1
#define REGM_LBL 2
#define REGM_DREG 3
#define REGM_DREG_DSP 4

typedef
struct amd64_mval {
	u8 type;
	u8 reg;
	u32 disp;
} amd64_mval_t;

typedef
struct amd64_instr {
	u8 op, var;
	u8 mod, reg_rm;
	u32 imm, disp;
} amd64_instr_t;

typedef
struct amd64_ctx {
	lt_arena_t* arena;

	isz curr_func;

	seg_ent_t* cs, *ds;
	seg_ent_t* ir_cs, *ir_ds;
	usz cs_count, ds_count;

	u8 reg_allocated[AMD64_REG_COUNT];

	amd64_mval_t reg_map[256];
} amd64_ctx_t;

void amd64_gen(amd64_ctx_t* cx);

void amd64_print_instr(amd64_instr_t instr);

#endif
