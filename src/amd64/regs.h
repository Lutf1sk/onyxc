#ifndef AMD64_REGS_H
#define AMD64_REGS_H 1

#include "../fwd.h"
#include <lt/lt.h>

#define REG_A	0b0000
#define REG_C	0b0001
#define REG_D	0b0010
#define REG_B	0b0011
#define REG_SP	0b0100
#define REG_BP	0b0101
#define REG_SI	0b0110
#define REG_DI	0b0111

#define REG_8	0b1000
#define REG_9	0b1001
#define REG_10	0b1010
#define REG_11	0b1011
#define REG_12	0b1100
#define REG_13	0b1101
#define REG_14	0b1110
#define REG_15	0b1111

#define REG_REX_BIT 0b1000

extern lstr_t reg_names[][4];

#define REG_ALLOCATABLE		1
#define REG_CALLER_OWNED	2
#define REG_SCRATCH			4
extern u8 reg_flags[];

u8 reg_alloc(amd64_ctx_t* cx, u32 ireg);
u8 reg_scratch(amd64_ctx_t* cx, u32 offs);

b8 reg_free(amd64_ctx_t* cx, u8 reg);
void zero_reg(amd64_ctx_t* cx, u8 reg);

#endif
