#include <lt/io.h>

#include "ops.h"
#include "amd64.h"
#include "common.h"
#include "regs.h"

#define OP(name, count, ...) { CLSTR(name), (amd64_var_t[]){__VA_ARGS__}, count }
#define VAR0(f, ...) { .arg_count = 0, .flags = (f), .instr = __VA_ARGS__ }
#define VAR1(a1, f, ...) { .args = {(a1), 0}, .arg_count = 1, .flags = (f), .instr = __VA_ARGS__ }
#define VAR2(a1, a2, f, ...) { .args = {(a1), (a2)}, .arg_count = 2, .flags = (f), .instr = __VA_ARGS__ }

#define UIMM (VARG_IMM)
#define SIMM (VARG_IMM|VARG_SGEXT)

#define UIMM8 (UIMM|VARG_8)
#define UIMM16 (UIMM|VARG_16)
#define UIMM32 (UIMM|VARG_32)
#define UIMM64 (UIMM|VARG_64)

#define SIMM8 (SIMM|VARG_8)
#define SIMM16 (SIMM|VARG_16)
#define SIMM32 (SIMM|VARG_32)
#define SIMM64 (SIMM|VARG_64)

#define REL8 (SIMM8|VARG_REL)
#define REL16 (SIMM16|VARG_REL)
#define REL32 (SIMM32|VARG_REL)
#define REL64 (SIMM64|VARG_REL)

#define REG8 (VARG_REG|VARG_8)
#define REG16 (VARG_REG|VARG_16)
#define REG32 (VARG_REG|VARG_32)
#define REG64 (VARG_REG|VARG_64)

#define RM8 (VARG_MRM|VARG_8)
#define RM16 (VARG_MRM|VARG_16)
#define RM32 (VARG_MRM|VARG_32)
#define RM64 (VARG_MRM|VARG_64)

amd64_op_t ops[] = {
	OP("add", 12,
		VAR2(RM8|VARG_DST,	REG8, 0, { 0x00 }),				// ADD r/m8, reg8
		VAR2(RM16|VARG_DST,	REG16, VFLAG_OPSIZE, { 0x01 }),	// ADD r/m16, reg16
		VAR2(RM32|VARG_DST,	REG32, 0, { 0x01 }),			// ADD r/m32, reg32
		VAR2(RM64|VARG_DST,	REG64, VFLAG_REX_W, { 0x01 }),	// ADD r/m64, reg64

		VAR2(REG8|VARG_DST,		RM8, 0, { 0x02 }),				// ADD reg8, r/m8
		VAR2(REG16|VARG_DST,	RM16, VFLAG_OPSIZE, { 0x03 }),	// ADD reg16, r/m16
		VAR2(REG32|VARG_DST,	RM32, 0, { 0x03 }),				// ADD reg32, r/m32
		VAR2(REG64|VARG_DST,	RM64, VFLAG_REX_W, { 0x03 }),	// ADD reg64, r/m64

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 0, 0x80 }),					// ADD r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 0, 0x81 }),	// ADD r/m16, uimm16
		VAR2(RM32|VARG_DST,	UIMM32, VFLAG_OP_EXT, { 0, 0x81 }),					// ADD r/m32, uimm32
		VAR2(RM64|VARG_DST,	SIMM32, VFLAG_REX_W|VFLAG_OP_EXT, { 0, 0x81 })),	// ADD r/m64, simm32

	OP("sub", 12,
		VAR2(RM8|VARG_DST,	REG8, 0, { 0x28 }),				// SUB r/m8, reg8
		VAR2(RM16|VARG_DST,	REG16, VFLAG_OPSIZE, { 0x29 }),	// SUB r/m16, reg16
		VAR2(RM32|VARG_DST,	REG32, 0, { 0x29 }),			// SUB r/m32, reg32
		VAR2(RM64|VARG_DST,	REG64, VFLAG_REX_W, { 0x29 }),	// SUB r/m64, reg64

		VAR2(REG8|VARG_DST,		RM8, 0, { 0x2A }),				// SUB reg8, r/m8
		VAR2(REG16|VARG_DST,	RM16, VFLAG_OPSIZE, { 0x2B }),	// SUB reg16, r/m16
		VAR2(REG32|VARG_DST,	RM32, 0, { 0x2B }),				// SUB reg32, r/m32
		VAR2(REG64|VARG_DST,	RM64, VFLAG_REX_W, { 0x2B }),	// SUB reg64, r/m64

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 5, 0x80 }),					// SUB r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 5, 0x81 }),	// SUB r/m16, uimm16
		VAR2(RM32|VARG_DST,	UIMM32, VFLAG_OP_EXT, { 5, 0x81 }),					// SUB r/m32, uimm32
		VAR2(RM64|VARG_DST,	SIMM32, VFLAG_REX_W|VFLAG_OP_EXT, { 5, 0x81 })),	// SUB r/m64, simm32

	OP("and", 12,
		VAR2(RM8|VARG_DST,	REG8, 0, { 0x20 }),				// AND r/m8, reg8
		VAR2(RM16|VARG_DST,	REG16, VFLAG_OPSIZE, { 0x21 }),	// AND r/m16, reg16
		VAR2(RM32|VARG_DST,	REG32, 0, { 0x21 }),			// AND r/m32, reg32
		VAR2(RM64|VARG_DST,	REG64, VFLAG_REX_W, { 0x21 }),	// AND r/m64, reg64

		VAR2(REG8|VARG_DST,		RM8, 0, { 0x22 }),				// AND reg8, r/m8
		VAR2(REG16|VARG_DST,	RM16, VFLAG_OPSIZE, { 0x23 }),	// AND reg16, r/m16
		VAR2(REG32|VARG_DST,	RM32, 0, { 0x23 }),				// AND reg32, r/m32
		VAR2(REG64|VARG_DST,	RM64, VFLAG_REX_W, { 0x23 }),	// AND reg64, r/m64

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 4, 0x80 }),					// AND r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 4, 0x81 }),	// AND r/m16, uimm16
		VAR2(RM32|VARG_DST,	UIMM32, VFLAG_OP_EXT, { 4, 0x81 }),					// AND r/m32, uimm32
		VAR2(RM64|VARG_DST,	SIMM32, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0x81 })),	// AND r/m64, simm32

	OP("or", 12,
		VAR2(RM8|VARG_DST,	REG8, 0, { 0x08 }),				// OR r/m8, reg8
		VAR2(RM16|VARG_DST,	REG16, VFLAG_OPSIZE, { 0x09 }),	// OR r/m16, reg16
		VAR2(RM32|VARG_DST,	REG32, 0, { 0x09 }),			// OR r/m32, reg32
		VAR2(RM64|VARG_DST,	REG64, VFLAG_REX_W, { 0x09 }),	// OR r/m64, reg64

		VAR2(REG8|VARG_DST,		RM8, 0, { 0x0A }),				// OR reg8, r/m8
		VAR2(REG16|VARG_DST,	RM16, VFLAG_OPSIZE, { 0x0B }),	// OR reg16, r/m16
		VAR2(REG32|VARG_DST,	RM32, 0, { 0x0B }),				// OR reg32, r/m32
		VAR2(REG64|VARG_DST,	RM64, VFLAG_REX_W, { 0x0B }),	// OR reg64, r/m64

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 1, 0x80 }),					// OR r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 1, 0x81 }),	// OR r/m16, uimm16
		VAR2(RM32|VARG_DST,	UIMM32, VFLAG_OP_EXT, { 1, 0x81 }),					// OR r/m32, uimm32
		VAR2(RM64|VARG_DST,	SIMM32, VFLAG_REX_W|VFLAG_OP_EXT, { 1, 0x81 })),	// OR r/m64, simm32

	OP("xor", 12,
		VAR2(RM8|VARG_DST,	REG8, 0, { 0x30 }),				// XOR r/m8, reg8
		VAR2(RM16|VARG_DST,	REG16, VFLAG_OPSIZE, { 0x31 }),	// XOR r/m16, reg16
		VAR2(RM32|VARG_DST,	REG32, 0, { 0x31 }),			// XOR r/m32, reg32
		VAR2(RM64|VARG_DST,	REG64, VFLAG_REX_W, { 0x31 }),	// XOR r/m64, reg64

		VAR2(REG8|VARG_DST,		RM8, 0, { 0x32 }),				// XOR reg8, r/m8
		VAR2(REG16|VARG_DST,	RM16, VFLAG_OPSIZE, { 0x33 }),	// XOR reg16, r/m16
		VAR2(REG32|VARG_DST,	RM32, 0, { 0x33 }),				// XOR reg32, r/m32
		VAR2(REG64|VARG_DST,	RM64, VFLAG_REX_W, { 0x33 }),	// XOR reg64, r/m64

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 6, 0x80 }),					// XOR r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 6, 0x81 }),	// XOR r/m16, uimm16
		VAR2(RM32|VARG_DST,	UIMM32, VFLAG_OP_EXT, { 6, 0x81 }),					// XOR r/m32, uimm32
		VAR2(RM64|VARG_DST,	SIMM32, VFLAG_REX_W|VFLAG_OP_EXT, { 6, 0x81 })),	// XOR r/m64, simm32

	OP("shl", 8,
		VAR1(RM8|VARG_DST, VFLAG_OP_EXT, { 4, 0xD2 }),					// SHL r/m8, CL
		VAR1(RM16|VARG_DST, VFLAG_OPSIZE|VFLAG_OP_EXT, { 4, 0xD3 }),	// SHL r/m16, CL
		VAR1(RM32|VARG_DST, VFLAG_OP_EXT, { 4, 0xD3 }),					// SHL r/m32, CL
		VAR1(RM64|VARG_DST, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xD3 }),		// SHL r/m64, CL

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 4, 0xC0 }),				// SHL r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM8, VFLAG_OPSIZE|VFLAG_OP_EXT, { 4, 0xC1 }),	// SHL r/m16, uimm8
		VAR2(RM32|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 4, 0xC1 }),				// SHL r/m32, uimm8
		VAR2(RM64|VARG_DST,	UIMM8, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xC1 })),	// SHL r/m64, uimm8

	OP("shr", 8,
		VAR1(RM8|VARG_DST, VFLAG_OP_EXT, { 5, 0xD2 }),					// SHR r/m8, CL
		VAR1(RM16|VARG_DST, VFLAG_OPSIZE|VFLAG_OP_EXT, { 5, 0xD3 }),	// SHR r/m16, CL
		VAR1(RM32|VARG_DST, VFLAG_OP_EXT, { 5, 0xD3 }),					// SHR r/m32, CL
		VAR1(RM64|VARG_DST, VFLAG_REX_W|VFLAG_OP_EXT, { 5, 0xD3 }),		// SHR r/m64, CL

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 5, 0xC0 }),				// SHR r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM8, VFLAG_OPSIZE|VFLAG_OP_EXT, { 5, 0xC1 }),	// SHR r/m16, uimm8
		VAR2(RM32|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 5, 0xC1 }),				// SHR r/m32, uimm8
		VAR2(RM64|VARG_DST,	UIMM8, VFLAG_REX_W|VFLAG_OP_EXT, { 5, 0xC1 })),	// SHR r/m64, uimm8

	OP("sal", 8,
		VAR1(RM8|VARG_DST, VFLAG_OP_EXT, { 4, 0xD2 }),					// SAL r/m8, CL
		VAR1(RM16|VARG_DST, VFLAG_OPSIZE|VFLAG_OP_EXT, { 4, 0xD3 }),	// SAL r/m16, CL
		VAR1(RM32|VARG_DST, VFLAG_OP_EXT, { 4, 0xD3 }),					// SAL r/m32, CL
		VAR1(RM64|VARG_DST, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xD3 }),		// SAL r/m64, CL

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 4, 0xC0 }),				// SAL r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM8, VFLAG_OPSIZE|VFLAG_OP_EXT, { 4, 0xC1 }),	// SAL r/m16, uimm8
		VAR2(RM32|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 4, 0xC1 }),				// SAL r/m32, uimm8
		VAR2(RM64|VARG_DST,	UIMM8, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xC1 })),	// SAL r/m64, uimm8

	OP("sar", 8,
		VAR1(RM8|VARG_DST, VFLAG_OP_EXT, { 7, 0xD2 }),					// SAR r/m8, CL
		VAR1(RM16|VARG_DST, VFLAG_OPSIZE|VFLAG_OP_EXT, { 7, 0xD3 }),	// SAR r/m16, CL
		VAR1(RM32|VARG_DST, VFLAG_OP_EXT, { 7, 0xD3 }),					// SAR r/m32, CL
		VAR1(RM64|VARG_DST, VFLAG_REX_W|VFLAG_OP_EXT, { 7, 0xD3 }),		// SAR r/m64, CL

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 7, 0xC0 }),				// SAR r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM8, VFLAG_OPSIZE|VFLAG_OP_EXT, { 7, 0xC1 }),	// SAR r/m16, uimm8
		VAR2(RM32|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 7, 0xC1 }),				// SAR r/m32, uimm8
		VAR2(RM64|VARG_DST,	UIMM8, VFLAG_REX_W|VFLAG_OP_EXT, { 7, 0xC1 })),	// SAR r/m64, uimm8

	OP("neg", 4,
		VAR1(RM8|VARG_DST, VFLAG_OP_EXT, { 3, 0xF6 }),					// NEG r/m8
		VAR1(RM16|VARG_DST, VFLAG_OPSIZE|VFLAG_OP_EXT, { 3, 0xF7 }),	// NEG r/m16
		VAR1(RM32|VARG_DST, VFLAG_OP_EXT, { 3, 0xF7 }),					// NEG r/m32
		VAR1(RM64|VARG_DST, VFLAG_REX_W|VFLAG_OP_EXT, { 3, 0xF7 })),	// NEG r/m64

	OP("inc", 4,
		VAR1(RM8|VARG_DST, VFLAG_OP_EXT, { 0, 0xFE }),					// INC r/m8
		VAR1(RM16|VARG_DST, VFLAG_OPSIZE|VFLAG_OP_EXT, { 0, 0xFF }),	// INC r/m16
		VAR1(RM32|VARG_DST, VFLAG_OP_EXT, { 0, 0xFF }),					// INC r/m32
		VAR1(RM64|VARG_DST, VFLAG_REX_W|VFLAG_OP_EXT, { 0, 0xFF })),	// INC r/m64

	OP("dec", 4,
		VAR1(RM8|VARG_DST, VFLAG_OP_EXT, { 1, 0xFE }),					// DEC r/m8
		VAR1(RM16|VARG_DST, VFLAG_OPSIZE|VFLAG_OP_EXT, { 1, 0xFF }),	// DEC r/m16
		VAR1(RM32|VARG_DST, VFLAG_OP_EXT, { 1, 0xFF }),					// DEC r/m32
		VAR1(RM64|VARG_DST, VFLAG_REX_W|VFLAG_OP_EXT, { 1, 0xFF })),	// DEC r/m64

	OP("mov", 12,
		VAR2(RM8|VARG_DST,	REG8, 0, { 0x88 }),				// MOV r/m8, reg8
		VAR2(RM16|VARG_DST,	REG16, VFLAG_OPSIZE, { 0x89 }),	// MOV r/m16, reg16
		VAR2(RM32|VARG_DST,	REG32, 0, { 0x89 }),			// MOV r/m32, reg32
		VAR2(RM64|VARG_DST,	REG64, VFLAG_REX_W, { 0x89 }),	// MOV r/m64, reg64

		VAR2(REG8|VARG_DST,		RM8, 0, { 0x8A }),				// MOV reg8, r/m8
		VAR2(REG16|VARG_DST,	RM16, VFLAG_OPSIZE, { 0x8B }),	// MOV reg16, r/m16
		VAR2(REG32|VARG_DST,	RM32, 0, { 0x8B }),				// MOV reg32, r/m32
		VAR2(REG64|VARG_DST,	RM64, VFLAG_REX_W, { 0x8B }),	// MOV reg64, r/m64

		VAR2(REG8|VARG_DST,		UIMM8, VFLAG_OI, { 0xB0 }),					// MOV reg8, uimm8
		VAR2(REG16|VARG_DST,	UIMM16, VFLAG_OI|VFLAG_OPSIZE, { 0xB8 }),	// MOV reg16, uimm16
		VAR2(REG32|VARG_DST,	UIMM32, VFLAG_OI, { 0xB8 }),				// MOV reg32, uimm32
		VAR2(REG64|VARG_DST,	UIMM64, VFLAG_OI|VFLAG_REX_W, { 0xB8 }),	// MOV reg64, uimm64

		VAR2(RM8|VARG_DST,	UIMM8, VFLAG_OP_EXT, { 0, 0xC6 }),					// MOV r/m8, uimm8
		VAR2(RM16|VARG_DST,	UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 0, 0xC7 }),	// MOV r/m16, uimm16
		VAR2(RM32|VARG_DST,	UIMM32, VFLAG_OP_EXT, { 0, 0xC7 }),					// MOV r/m32, uimm32
		VAR2(RM64|VARG_DST,	SIMM32, VFLAG_REX_W|VFLAG_OP_EXT, { 0, 0xC7 })),	// MOV r/m64, simm32

	OP("lea", 3,
		VAR2(REG16|VARG_DST,	RM64, VFLAG_OPSIZE, { 0x8D }),	// LEA r16, m
		VAR2(REG32|VARG_DST,	RM64, 0, { 0x8D }),				// LEA r32, m
		VAR2(REG64|VARG_DST,	RM64, VFLAG_REX_W, { 0x8D })),	// LEA r64, m

	OP("movzx", 6,
		VAR2(REG16|VARG_DST,	RM8, VFLAG_OPSIZE, { 0x0F, 0xB6 }),	// MOVZX reg16, r/m8
		VAR2(REG32|VARG_DST,	RM8, 0, { 0x0F, 0xB6 }),			// MOVZX reg32, r/m8
		VAR2(REG64|VARG_DST,	RM8, VFLAG_REX_W, { 0x0F, 0xB6 }),	// MOVZX reg64, r/m8

		VAR2(REG32|VARG_DST,	RM16, 0, { 0x0F, 0xB7 }),			// MOVZX reg32, r/m16
		VAR2(REG64|VARG_DST,	RM16, VFLAG_REX_W, { 0x0F, 0xB7 }),	// MOVZX reg64, r/m16

		VAR2(REG64|VARG_DST,	RM32, 0, { 0x8B })),				// MOV reg32, r/m32

	OP("movsx", 6,
		VAR2(REG16|VARG_DST,	RM8, VFLAG_OPSIZE, { 0x0F, 0xBE }),	// MOVSX reg16, r/m8
		VAR2(REG32|VARG_DST,	RM8, 0, { 0x0F, 0xBE }),			// MOVSX reg32, r/m8
		VAR2(REG64|VARG_DST,	RM8, VFLAG_REX_W, { 0x0F, 0xBE }),	// MOVSX reg64, r/m8

		VAR2(REG32|VARG_DST,	RM16, 0, { 0x0F, 0xBF }),			// MOVSX reg32, r/m16
		VAR2(REG64|VARG_DST,	RM16, VFLAG_REX_W, { 0x0F, 0xBF }),	// MOVSX reg64, r/m16

		VAR2(REG64|VARG_DST,	RM32, 0, { 0x63 })),				// MOVSXD reg64, r/m32

	OP("jmp", 2,
		VAR1(REL32, 0, { 0xE9 }),
		VAR1(RM64, VFLAG_OP_EXT, { 4, 0xFF })),

	OP("call", 2,
		VAR1(REL32, 0, { 0xE8 }),
		VAR1(RM64, VFLAG_OP_EXT, { 2, 0xFF })),

	OP("ret", 1,
		VAR0(0, { 0xC3 })),

	OP("syscall", 1,
		VAR0(0, { 0x0F, 0x05 })),

	OP("ir_enter", 0),
	OP("ir_ret", 0),

	OP("div", 4,
		VAR1(RM8, VFLAG_OP_EXT, { 6, 0xF6 }),				// DIV r/m8
		VAR1(RM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 6, 0xF7 }),	// DIV r/m16
		VAR1(RM32, VFLAG_OP_EXT, { 6, 0xF7 }),				// DIV r/m32
		VAR1(RM64, VFLAG_REX_W|VFLAG_OP_EXT, { 6, 0xF7 })),	// DIV r/m64

	OP("idiv", 4,
		VAR1(RM8, VFLAG_OP_EXT, { 7, 0xF6 }),				// IDIV r/m8
		VAR1(RM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 7, 0xF7 }),	// IDIV r/m16
		VAR1(RM32, VFLAG_OP_EXT, { 7, 0xF7 }),				// IDIV r/m32
		VAR1(RM64, VFLAG_REX_W|VFLAG_OP_EXT, { 7, 0xF7 })),	// IDIV r/m64

	OP("cqo", 3,
		VAR0(VFLAG_OPSIZE, { 0x99 }),	// CWD
		VAR0(0, { 0x99 }),				// CDQ
		VAR0(VFLAG_REX_W, { 0x99 })),	// CQO

	OP("mul", 4,
		VAR1(RM8, VFLAG_OP_EXT, { 4, 0xF6 }),				// MUL r/m8
		VAR1(RM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 4, 0xF7 }),	// MUL r/m16
		VAR1(RM32, VFLAG_OP_EXT, { 4, 0xF7 }),				// MUL r/m32
		VAR1(RM64, VFLAG_REX_W|VFLAG_OP_EXT, { 4, 0xF7 })),	// MUL r/m64

	OP("imul", 4,
		VAR1(RM8, VFLAG_OP_EXT, { 5, 0xF6 }),				// IMUL r/m8
		VAR1(RM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 5, 0xF7 }),	// IMUL r/m16
		VAR1(RM32, VFLAG_OP_EXT, { 5, 0xF7 }),				// IMUL r/m32
		VAR1(RM64, VFLAG_REX_W|VFLAG_OP_EXT, { 5, 0xF7 })),	// IMUL r/m64

	OP("movsb", 1,
		VAR0(VFLAG_REX_W, { 0xA4 })),
	OP("movsq", 1,
		VAR0(VFLAG_REX_W, { 0xA5 })),

	OP("int", 1,
		VAR1(UIMM8, 0, { 0xCD })),

	OP("seta", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x97 })),
	OP("setae", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x93 })),
	OP("setb", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x92 })),
	OP("setbe", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x96 })),
	OP("sete", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x94 })),
	OP("setg", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x9F })),
	OP("setge", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x9D })),
	OP("setl", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x9C })),
	OP("setle", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x9E })),
	OP("setne", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x95 })),
	OP("setnz", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x95 })),
	OP("setz", 1,
		VAR1(RM8|VARG_DST, 0, { 0x0F, 0x94 })),

	OP("ja", 1,
		VAR1(REL32, 0, { 0x0F, 0x87 })),
	OP("jae", 1,
		VAR1(REL32, 0, { 0x0F, 0x83 })),
	OP("jb", 1,
		VAR1(REL32, 0, { 0x0F, 0x82 })),
	OP("jbe", 1,
		VAR1(REL32, 0, { 0x0F, 0x86 })),
	OP("je", 1,
		VAR1(REL32, 0, { 0x0F, 0x84 })),
	OP("jg", 1,
		VAR1(REL32, 0, { 0x0F, 0x8F })),
	OP("jge", 1,
		VAR1(REL32, 0, { 0x0F, 0x8D })),
	OP("jl", 1,
		VAR1(REL32, 0, { 0x0F, 0x8C })),
	OP("jle", 1,
		VAR1(REL32, 0, { 0x0F, 0x8E })),
	OP("jne", 1,
		VAR1(REL32, 0, { 0x0F, 0x85 })),
	OP("jnz", 1,
		VAR1(REL32, 0, { 0x0F, 0x85 })),
	OP("jz", 1,
		VAR1(REL32, 0, { 0x0F, 0x84 })),

	OP("cmp", 12,
		VAR2(RM8,	REG8, 0, { 0x38 }),				// CMP r/m8, reg8
		VAR2(RM16,	REG16, VFLAG_OPSIZE, { 0x39 }),	// CMP r/m16, reg16
		VAR2(RM32,	REG32, 0, { 0x39 }),			// CMP r/m32, reg32
		VAR2(RM64,	REG64, VFLAG_REX_W, { 0x39 }),	// CMP r/m64, reg64

		VAR2(REG8,	RM8, 0, { 0x3A }),				// CMP reg8, r/m8
		VAR2(REG16,	RM16, VFLAG_OPSIZE, { 0x3B }),	// CMP reg16, r/m16
		VAR2(REG32,	RM32, 0, { 0x3B }),				// CMP reg32, r/m32
		VAR2(REG64,	RM64, VFLAG_REX_W, { 0x3B }),	// CMP reg64, r/m64

		VAR2(RM8,	UIMM8, VFLAG_OP_EXT, { 7, 0x80 }),					// CMP r/m8, uimm8
		VAR2(RM16,	UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 7, 0x81 }),	// CMP r/m16, uimm16
		VAR2(RM32,	UIMM32, VFLAG_OP_EXT, { 7, 0x81 }),					// CMP r/m32, uimm32
		VAR2(RM64,	SIMM32, VFLAG_REX_W|VFLAG_OP_EXT, { 7, 0x81 })),	// CMP r/m64, simm32

	OP("test", 8,
		VAR2(RM8,	REG8, 0, { 0x84 }),				// TEST r/m8, reg8
		VAR2(RM16,	REG16, VFLAG_OPSIZE, { 0x85 }),	// TEST r/m16, reg16
		VAR2(RM32,	REG32, 0, { 0x85 }),			// TEST r/m32, reg32
		VAR2(RM64,	REG64, VFLAG_REX_W, { 0x85 }),	// TEST r/m64, reg64

		VAR2(RM8,	UIMM8, VFLAG_OP_EXT, { 0, 0xF6 }),					// TEST r/m8, uimm8
		VAR2(RM16,	UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 0, 0xF7 }),	// TEST r/m16, uimm16
		VAR2(RM32,	UIMM32, VFLAG_OP_EXT, { 0, 0xF7 }),				// TEST r/m32, uimm32
		VAR2(RM64,	SIMM32, VFLAG_REX_W|VFLAG_OP_EXT, { 0, 0xF7 })),	// TEST r/m64, simm32

	OP("push", 5,
		VAR1(RM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 6, 0xFF }),	// PUSH r/m16
		VAR1(RM64, VFLAG_OP_EXT, { 6, 0xFF }),				// PUSH r/m64

		VAR1(UIMM8, VFLAG_OP_EXT, { 6, 0x6A }),					// PUSH uimm8
		VAR1(UIMM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 6, 0x68 }),	// PUSH uimm16
		VAR1(UIMM32, VFLAG_OP_EXT, { 6, 0x68 })),				// PUSH uimm32

	OP("pop", 2,
		VAR1(RM16, VFLAG_OPSIZE|VFLAG_OP_EXT, { 0, 0x8F }),	// POP r/m16
		VAR1(RM64, VFLAG_OP_EXT, { 0, 0x8F })),				// POP r/m64
};

void zero_reg(amd64_ctx_t* cx, u8 mreg) {
	amd64_ireg_t args[2] = { XREG(mreg, 8), XREG(mreg, 8) };
	emit_instr(cx, X64_XOR, 2, args);
}

void x64_mov(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2) {
	LT_ASSERT(!ireg_reg_displaced(&v1));

	if (ireg_eq(&v1, &v2))
		return;

	if (v1.type == IREG_REG && v2.type == IREG_IMM && !(v2.imm + v2.disp)) {
		zero_reg(cx, v1.mreg);
		return;
	}

	if (ireg_reg_pure(&v1) && ireg_reg_displaced(&v2)) {
		v2.type |= IREG_REF;
		x64_lea(cx, v1, v2);
		return;
	}

	amd64_ireg_t args[2] = {v1, v2};
	emit_instr(cx, X64_MOV, 2, args);
}

void x64_movzx(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2) {
	if (v1.size <= v2.size) {
		v2.size = v1.size;
		x64_mov(cx, v1, v2);
		return;
	}

	amd64_ireg_t args[2] = { v1, v2 };
	emit_instr(cx, X64_MOVZX, 2, args);
}

void x64_movsx(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2) {
	if (v1.size <= v2.size) {
		v2.size = v1.size;
		x64_mov(cx, v1, v2);
		return;
	}

	amd64_ireg_t args[2] = { v1, v2 };
	emit_instr(cx, X64_MOVSX, 2, args);
}

void x64_lea(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2) {
	LT_ASSERT(v2.type & IREG_REF);
	LT_ASSERT(ireg_reg_pure(&v1));

	v2.size = 8;
	if (v1.size < 2) {
		v1.size = 2;
	}

	amd64_ireg_t args[2] = {v1, v2};
	emit_instr(cx, X64_LEA, 2, args);
}

