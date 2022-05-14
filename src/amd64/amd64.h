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
	u8 mreg;
	u32 disp;
	union {
		u32 imm;
		u32 seg;
	};
} amd64_ireg_t;

typedef
struct amd64_stack_val {
	usz offs;
	usz size;
	usz align;
} amd64_stack_val_t;

typedef
struct amd64_ctx {
	lt_arena_t* arena;

	isz curr_func;

	seg_ent_t* seg;
	usz seg_count;

	u32 reg_allocated[AMD64_REG_COUNT];

	usz stack_val_count;
	usz stack_val_it;
	amd64_stack_val_t* stack_layout;

	usz* reg_lifetimes;
	amd64_ireg_t* reg_map;

	usz arg_num;
} amd64_ctx_t;

void amd64_gen(amd64_ctx_t* cx);
void amd64_print_seg(amd64_ctx_t* cx, usz i);
void amd64_print_instr(amd64_instr_t* instr);

void amd64_write_elf64(amd64_ctx_t* cx, char* path);

#endif
