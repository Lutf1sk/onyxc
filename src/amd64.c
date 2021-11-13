#include <lt/io.h>

#include "amd64.h"
#include "interm.h"
#include "segment.h"

#define REG_A	0b000
#define REG_C	0b001
#define REG_D	0b010
#define REG_B	0b011
#define REG_SP	0b100
#define REG_BP	0b101
#define REG_SI	0b110
#define REG_DI	0b111

#define REG_8	0b1000
#define REG_9	0b1001
#define REG_10	0b1010
#define REG_11	0b1011
#define REG_12	0b1100
#define REG_13	0b1101
#define REG_14	0b1110
#define REG_15	0b1111

#define REG_REX 0b1000

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
#define REX(w, r, x, b) ((0b0100 << 4) | (w << 3) | (r << 2) | (x << 1) | b)
#define REX_W (1 << 3)
#define REX_R (1 << 2)
#define REX_X (1 << 1)
#define REX_B (1 << 0)

// MMRRRMMM
#define MODRM(mod, reg, rm) ((mod << 6) | (reg << 3) | rm)

// SSIIIBBB
#define SIB(scale, index, base) ((scale << 6) | (index << 3) | base)
#define SIB_S1 0
#define SIB_S2 1
#define SIB_S4 2
#define SIB_S8 3

static
b8 ival_eq(ival_t* v1, ival_t* v2) {
	if (v1->stype != v2->stype || v1->size != v2->size || v1->disp != v2->disp)
		return 0;

	switch (v1->stype & ~IVAL_REF) {
	case IVAL_REG: return v1->reg == v2->reg;
	case IVAL_IMM: return v1->uint_val == v2->uint_val;
	case IVAL_DSO: return v1->dso == v2->dso && v1->disp != v2->disp;
	case IVAL_CSO: return v1->cso == v2->cso && v1->disp != v2->disp;
	}
	return 0;
}

#define DIR_REG 1
#define DIR_RM 2

static
u8 lookup_reg(amd64_ctx_t* cx, usz reg) {
	LT_ASSERT(reg < 256);

	return reg & 0xF;
}

static
isz write_ival_modrm(amd64_ctx_t* cx, u8* out, u8* rex, u8 reg, ival_t* iv) {
	u8* start = out;

	u8 mod, rm;
	i32 disp = 0;

	switch (iv->stype) {
	case IVAL_REG | IVAL_REF:
		mod = 0b00; // Indirect, no displacement
		rm = lookup_reg(cx, iv->reg);
		break;

	case IVAL_DSO | IVAL_REF: case IVAL_CSO | IVAL_REF:
		mod = 0b10; // Indirect, 32-bit displacement
		rm = REG_13;
		disp = iv->dso + iv->disp;
		break;

	case IVAL_REG:
		mod = 0b11; // Direct, register
		rm = lookup_reg(cx, iv->reg);
		break;

	default:
		return -1;
	}

	// Write ModRM
	if (reg & REG_REX) {
		reg &= ~REG_REX;
		*rex |= REX_R;
	}
	if (rm & REG_REX) {
		rm &= ~REG_REX;
		*rex |= REX_B;
	}
	*out++ = MODRM(mod, reg, rm);

	// Write displacement if there is any
	if (disp) {
		if (disp > 127) {
			*(u32*)out = disp;
			out += 4;
		}
		else
			*out++ = disp;
	}

	return out - start;
}

static
void gen_instr(amd64_ctx_t* cx, icode_t* ir) {
	switch (ir->op) {
	case IR_ADD: case IR_SUB: case IR_USHL: case IR_USHR: case IR_ISHL: case IR_ISHR:
	case IR_AND: case IR_OR: case IR_XOR:
		break;

	case IR_IREM: case IR_UREM: case IR_IDIV: case IR_UDIV:
		break;

	case IR_IMUL: case IR_UMUL:
		break;

	case IR_NEG:
		break;

	case IR_INC: case IR_DEC:
		break;

	case IR_ENTER: case IR_RET: case IR_GETARG: case IR_SETARG: case IR_SYSCALL:
		break;

	case IR_SRESV:
	case IR_MOV: case IR_LEA:
		break;

	case IR_IEXT: case IR_UEXT:
		break;

	case IR_CSETG: case IR_CSETGE: case IR_CSETL: case IR_CSETLE:
	case IR_CSETA: case IR_CSETAE: case IR_CSETB: case IR_CSETBE:
	case IR_CSETE: case IR_CSETNE: case IR_CSETZ: case IR_CSETNZ:
		break;

	case IR_CJMPG: case IR_CJMPGE: case IR_CJMPL: case IR_CJMPLE:
	case IR_CJMPA: case IR_CJMPAE: case IR_CJMPB: case IR_CJMPBE:
	case IR_CJMPE: case IR_CJMPNE: case IR_CJMPZ: case IR_CJMPNZ:
		break;

	case IR_CALL: case IR_JMP:
		break;

	default:
		lt_printf("%S\n", icode_type_str(ir->op));
		LT_ASSERT_NOT_REACHED();
	}
}

void amd64_gen(amd64_ctx_t* cx) {
	for (usz i = 0; i < cx->ir_cs_count; ++i) {
		seg_ent_t* cs = &cx->ir_cs[i];
		for (usz j = 0; j < cs->size; ++j) {
			icode_t* ic = &((icode_t*)cs->data)[j];
			gen_instr(cx, ic);
		}
	}
}

