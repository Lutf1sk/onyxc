#ifndef AMD64_COMMON_H
#define AMD64_COMMON_H 1

#include <lt/lt.h>

#include "../fwd.h"

#define PFX_LOCK	0xF0
#define PFX_REPNZ	0xF2
#define PFX_REP		0xF3
#define PFX_OPSZ	0x66
#define PFX_ADDRSZ	0x67

// 0100WRXB
// W - Promotes to 64-bit
// R - Extends ModRM reg
// X - Extends SIB index
// B - Extends ModRM rm
#define REX(w, r, x, b) ((0b0100 << 4) | ((w) << 3) | ((r) << 2) | ((x) << 1) | (b))
#define REX_W (1 << 3)
#define REX_R (1 << 2)
#define REX_X (1 << 1)
#define REX_B (1 << 0)

// ModR/M |    AX..BX    |       SP      |      BP      |  SI/DI  |
//--------|--------------|---------------|--------------|---------|
// Mod 00 |     [rm]     |     [SIB]     | [IP + dsp32] |  [rm]   |
//--------|--------------|---------------|--------------|---------|
// Mod 01 | [rm + dsp8]  | [SIB + dsp8]  |      [rm + dsp8]       |
//--------|--------------|---------------|------------------------|
// Mod 10 | [rm + dsp32] | [SIB + dsp32] |      [rm + dsp32]      |
//--------|--------------|---------------|------------------------|
// Mod 11 |                          rm                           |

// MMRRRMMM
#define MODRM(mod, reg, rm) (((mod) << 6) | ((reg) << 3) | (rm))
#define MOD_DREG	0b00
#define MOD_DSP8	0b01
#define MOD_DSP32	0b10
#define MOD_REG		0b11

// SIB - Mod 00
//         | AX..SP/SI..R12/R14/R15 |        BP/R13       | B.base
//---------|------------------------|---------------------|
// AX..BX  |  [B.base + X.index*s]  | [X.index*s + dsp32] |
//---------|------------------------|---------------------|
// SP      |         [B.base]       |       [dsp32]       |
//---------|------------------------|---------------------|
// BP..R15 |  [B.base + X.index*s]  | [X.index*s + dsp32] |
// X.index

// SIB - Mod 01/10
//         |         AX..BX/BP..R15         |         SP         | B.base
//---------|--------------------------------|--------------------|
// AX..R15 | [B.base + X.index*s + dsp8/32] | [B.base + dsp8/32] |
// X.index

// SSIIIBBB
#define SIB(scale, index, base) (((scale) << 6) | ((index) << 3) | (base))
#define SIB_S1 0
#define SIB_S2 1
#define SIB_S4 2
#define SIB_S8 3

#define IREG_REG 0
#define IREG_SEG 1
#define IREG_IMM 2
#define IREG_REF 4

#define IREG_INIT(type, ...) { (type), __VA_ARGS__ }
#define IREG(type, ...) ((amd64_ireg_t)IREG_INIT(type, __VA_ARGS__))
#define XREG(reg) IREG(IREG_REG, .mreg = (reg))
#define XIMMI(val) IREG(IREG_IMM, .imm = (val))
#define XSEG(i) IREG(IREG_SEG, .seg = (i))

#define MI_SEG 1

typedef
struct amd64_instr {
	u8 flags[2];
	u8 prefix[2];
	u8 op, var;
	u8 mod, reg_rm;
	u32 imm, disp;
} amd64_instr_t;

#define IREG_CONST

usz emit(amd64_ctx_t* cx, amd64_instr_t instr);

usz new_mcode_seg(amd64_ctx_t* cx, type_t* type, lstr_t name, u32 origin);

#endif
