#ifndef AMD64_H
#define AMD64_H 1

#include "../fwd.h"

#include <lt/fwd.h>
#include <lt/lt.h>

// amd64.c

#define AMD64_REG_COUNT 16

typedef
struct amd64_ireg {
	u8 type;
	u8 size;
	u8 mreg;
	u8 pad;
	u32 disp;
	union {
		u32 imm;
		u32 seg;
		u32 lbl;
	};
} amd64_ireg_t;

typedef
struct amd64_lbl {
	u32 i;
	u32 m_i;
	usz ref_count;
	u32* refs;
	struct amd64_lbl* next;
} amd64_lbl_t;

typedef
struct amd64_ctx {
	lt_arena_t* arena;

	isz curr_func;

	seg_ent_t* seg;
	usz seg_count;

	u32 reg_allocated[AMD64_REG_COUNT];

	usz stack_top;
	usz frame_size;

	usz* reg_lifetimes;
	amd64_ireg_t* reg_map;

	amd64_lbl_t* lbl, *lbl_it;

	usz arg_ir_indices[32];
	usz arg_num, arg_index_max;
} amd64_ctx_t;

void amd64_gen(amd64_ctx_t* cx);
void amd64_print_seg(amd64_ctx_t* cx, usz i);
void amd64_print_instr(amd64_ctx_t* cx, amd64_instr_t* instr);

void amd64_write_elf64(amd64_ctx_t* cx, char* path);

#endif
