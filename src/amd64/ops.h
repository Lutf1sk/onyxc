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
#define X64_IR_RET 21

#define X64_DIV 22
#define X64_IDIV 23
#define X64_CQO 24
#define X64_MUL 25
#define X64_IMUL 26

#define X64_MOVSB 27
#define X64_MOVSW 28

#define X64_INT 29

#define VARG_8		0b000000
#define VARG_16		0b000001
#define VARG_32		0b000010
#define VARG_64		0b000011
#define VARG_REG	0b000000
#define VARG_MRM	0b000100
#define VARG_IMM	0b001000
#define VARG_IP_REL	0b010000
#define VARG_NIP	0b100000

#define VARG_SIZE_MASK 0b000011
#define VARG_TYPE_MASK 0b001100
#define VARG_FLAG_MASK 0b110000

#define VFLAG_OP_EXT	0b0100
#define VFLAG_REX_W		0b1000

typedef
struct amd64_var {
	u8 args[2];
	u8 flags;
	u8 arg_count;

	u8 instr[8];
} amd64_var_t;

typedef
struct amd64_op {
	lstr_t str;
	amd64_var_t* vars;
	u8 var_count;
} amd64_op_t;

extern amd64_op_t ops[];

#endif
