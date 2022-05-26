#include "ops.h"

#define OP(name, count, ...) { CLSTR(name), (amd64_var_t[]){__VA_ARGS__}, count }
#define VAR0(f, ...) { .arg_count = 0, .flags = (f), .instr = __VA_ARGS__ }
#define VAR1(a1, f, ...) { .args = {(a1), 0}, .arg_count = 1, .flags = (f), .instr = __VA_ARGS__ }
#define VAR2(a1, a2, f, ...) { .args = {(a1), (a2)}, .arg_count = 2, .flags = (f), .instr = __VA_ARGS__ }

amd64_op_t ops[] = {
	OP("add", 3,
		VAR2(VARG_REG|VARG_64, VARG_MRM|VARG_64, VFLAG_REX_W, { 0x01 }),
		VAR2(VARG_MRM|VARG_64, VARG_REG|VARG_64, VFLAG_REX_W, { 0x03 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_32, VFLAG_REX_W|VFLAG_OP_EXT, { 0, 0x81 })),

	OP("sub", 3,
		VAR2(VARG_REG|VARG_64, VARG_MRM|VARG_64, VFLAG_REX_W, { 0x29 }),
		VAR2(VARG_MRM|VARG_64, VARG_REG|VARG_64, VFLAG_REX_W, { 0x2B }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_32, VFLAG_REX_W|VFLAG_OP_EXT, { 5, 0x81 })),

	OP("and", 3,
		VAR2(VARG_REG|VARG_64, VARG_MRM|VARG_64, VFLAG_REX_W, { 0x23 }),
		VAR2(VARG_MRM|VARG_64, VARG_REG|VARG_64, VFLAG_REX_W, { 0x21 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_32, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0x81 })),

	OP("or", 3,
		VAR2(VARG_REG|VARG_64, VARG_MRM|VARG_64, VFLAG_REX_W, { 0x0B }),
		VAR2(VARG_MRM|VARG_64, VARG_REG|VARG_64, VFLAG_REX_W, { 0x09 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_32, VFLAG_REX_W|VFLAG_OP_EXT, { 1, 0x81 })),

	OP("xor", 3,
		VAR2(VARG_REG|VARG_64, VARG_MRM|VARG_64, VFLAG_REX_W, { 0x33 }),
		VAR2(VARG_MRM|VARG_64, VARG_REG|VARG_64, VFLAG_REX_W, { 0x31 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_32, VFLAG_REX_W|VFLAG_OP_EXT, { 6, 0x81 })),

	OP("shl", 2,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xD3 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_8, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xC1 })),

	OP("shr", 2,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 5, 0xD3 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_8, VFLAG_REX_W|VFLAG_OP_EXT, { 5, 0xC1 })),

	OP("sal", 2,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xD3 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_8, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xC1 })),

	OP("sar", 2,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 7, 0xD3 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_8, VFLAG_REX_W|VFLAG_OP_EXT, { 7, 0xC1 })),

	OP("neg", 1,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 3, 0xF7 })),

	OP("inc", 1,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 0, 0xFF })),

	OP("inc", 1,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 1, 0xFF })),

	OP("mov", 3,
		VAR2(VARG_REG|VARG_64, VARG_MRM|VARG_64, VFLAG_REX_W, { 0x8B }),
		VAR2(VARG_MRM|VARG_64, VARG_REG|VARG_64, VFLAG_REX_W, { 0x89 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_32, VFLAG_REX_W|VFLAG_OP_EXT, { 0, 0xC7 })),

	OP("lea", 1,
		VAR2(VARG_REG|VARG_64, VARG_MRM|VARG_64, VFLAG_REX_W, { 0x8D })),

	OP("movzx", 0),
	OP("movsx", 0),

	OP("jmp", 2,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0xE9 }),
		VAR1(VARG_MRM|VARG_64, VFLAG_OP_EXT, { 4, 0xFF })),

	OP("call", 2,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0xE8 }),
		VAR1(VARG_MRM|VARG_64, VFLAG_OP_EXT, { 2, 0xFF })),

	OP("ret", 1,
		VAR0(0, { 0xC3 })),

	OP("syscall", 1,
		VAR0(0, { 0x0F, 0x05 })),

	OP("ir_enter", 0),
	OP("ir_ret", 0),

	OP("div", 1,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 6, 0xF7 })),

	OP("idiv", 1,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 7, 0xF7 })),

	OP("cqo", 1,
		VAR0(VFLAG_REX_W, { 0x99 })),

	OP("mul", 1,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xF7 })),

	OP("imul", 1,
		VAR1(VARG_MRM|VARG_64, VFLAG_REX_W|VFLAG_OP_EXT, { 5, 0xF7 })),

	OP("movsb", 0), // 0xA4
	OP("movsw", 0), // 0xA5

	OP("int", 1,
		VAR1(VARG_IMM|VARG_8, 0, { 0xCD })),

	OP("seta", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x97 })),
	OP("setae", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x93 })),
	OP("setb", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x92 })),
	OP("setbe", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x96 })),
	OP("sete", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x94 })),
	OP("setg", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x9F })),
	OP("setge", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x9D })),
	OP("setl", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x9C })),
	OP("setle", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x9E })),
	OP("setne", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x95 })),
	OP("setnz", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x95 })),
	OP("setz", 1,
		VAR1(VARG_MRM|VARG_8, 0, { 0x0F, 0x94 })),

	OP("ja", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x87 })),
	OP("jae", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x83 })),
	OP("jb", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x82 })),
	OP("jbe", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x86 })),
	OP("je", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x84 })),
	OP("jg", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x8F })),
	OP("jge", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x8D })),
	OP("jl", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x8C })),
	OP("jle", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x8E })),
	OP("jne", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x85 })),
	OP("jnz", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x85 })),
	OP("jz", 1,
		VAR1(VARG_IMM|VARG_32|VARG_REL, 0, { 0x0F, 0x84 })),

	OP("cmp", 3,
		VAR2(VARG_REG|VARG_64, VARG_MRM|VARG_64, VFLAG_REX_W, { 0x3B }),
		VAR2(VARG_MRM|VARG_64, VARG_REG|VARG_64, VFLAG_REX_W, { 0x39 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_32, VFLAG_REX_W|VFLAG_OP_EXT, { 7, 0x81 })),

	OP("test", 2,
		VAR2(VARG_MRM|VARG_64, VARG_REG|VARG_64, VFLAG_REX_W, { 0x85 }),
		VAR2(VARG_MRM|VARG_64, VARG_IMM|VARG_32, VFLAG_REX_W|VFLAG_OP_EXT, { 0, 0xF7 })),
};

