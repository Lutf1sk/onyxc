#ifndef AMD64_OPS_H
#define AMD64_OPS_H 1

#include "../fwd.h"
#include <lt/lt.h>

#define X64_ADD 0
#define X64_SUB 1
#define X64_AND 2
#define X64_OR 3
#define X64_XOR 4
#define X64_SHL 5
#define X64_SHR 6
#define X64_SAL 7
#define X64_SAR 8

#define X64_NEG 9
#define X64_INC 10
#define X64_DEC 11

#define X64_MOV 12
#define X64_LEA 13
#define X64_MOVZX 14
#define X64_MOVSX 15

#define X64_JMP 16
#define X64_CALL 17
#define X64_RET 18

#define X64_SYSCALL 19

#define X64_IR_ENTER 20
#define X64_IR_LEAVE 21

#define X64_DIV 22
#define X64_IDIV 23
#define X64_CQO 24
#define X64_MUL 25
#define X64_IMUL 26

#define VMOD_REG 0b00
#define VMOD_MRM 0b01
#define VMOD_IMM 0b10

#define VARG_8	0
#define VARG_16	1
#define VARG_32	2
#define VARG_64	3

#define VARG_OP_EXT 0b01

#define VARG(n, x) (x << ((n) * 2 + 2))
#define VARG_GET(x, n) (((x) >> ((n) * 2 + 2)) & 0b11)

typedef
struct amd64_var {
	u8 args;
	u8 sizes;
} amd64_var_t;

typedef
struct amd64_op {
	lstr_t str;

	// Variations
	amd64_var_t* vars;
	u8 (*var_ops)[4];
	u8 var_count;
} amd64_op_t;

extern amd64_op_t ops[];

#endif
