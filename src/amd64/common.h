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

// MMRRRMMM
#define MODRM(mod, reg, rm) (((mod) << 6) | ((reg) << 3) | (rm))
#define MOD_DREG	0b00
#define MOD_DSP8	0b01
#define MOD_DSP32	0b10
#define MOD_REG		0b11

// SSIIIBBB
#define SIB(scale, index, base) (((scale) << 6) | ((index) << 3) | (base))
#define SIB_S1 0
#define SIB_S2 1
#define SIB_S4 2
#define SIB_S8 3

#define MVAL_INVAL 0
#define MVAL_REG 1
#define MVAL_LBL 2
#define MVAL_DREG 3
#define MVAL_DREG_DSP 4
#define MVAL_DLBL 5
#define MVAL_IMM 6
#define MVAL_DIMM 7

typedef
struct amd64_mval {
	u8 stype;
	u8 reg;
	u16 flags;
	u32 disp;
} amd64_mval_t;

#define IREG_CONST

usz emit(amd64_ctx_t* cx, amd64_instr_t instr);

usz new_mcode_seg(amd64_ctx_t* cx, type_t* type);

#endif
