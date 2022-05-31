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

#define X64_SETA	30
#define X64_SETAE	31
#define X64_SETB	32
#define X64_SETBE	33
#define X64_SETE	34
#define X64_SETG	35
#define X64_SETGE	36
#define X64_SETL	37
#define X64_SETLE	38
#define X64_SETNE	39
#define X64_SETNZ	40
#define X64_SETZ	41

#define X64_JA	42
#define X64_JAE	43
#define X64_JB	44
#define X64_JBE	45
#define X64_JE	46
#define X64_JG	47
#define X64_JGE	48
#define X64_JL	49
#define X64_JLE	50
#define X64_JNE	51
#define X64_JNZ	52
#define X64_JZ	53

#define X64_CMP 54
#define X64_TEST 55

#define X64_PUSH 56
#define X64_POP 57

#define VARG_8		0b00000000
#define VARG_16		0b00000001
#define VARG_32		0b00000010
#define VARG_64		0b00000011
#define VARG_REG	0b00000000
#define VARG_MRM	0b00000100
#define VARG_IMM	0b00001000
#define VARG_REL	0b00010000
#define VARG_DST	0b00100000
#define VARG_SGEXT	0b01000000

#define VARG_SIZE_MASK 0b00000011
#define VARG_TYPE_MASK 0b00001100
#define VARG_FLAG_MASK 0b11110000

#define VFLAG_OP_EXT	0b0001
#define VFLAG_REX_W		0b0010
#define VFLAG_OPSIZE	0b0100

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

void zero_reg(amd64_ctx_t* cx, u8 mreg);

void x64_mov(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2);
void x64_movzx(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2);
void x64_movsx(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2);
void x64_lea(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2);

#endif
