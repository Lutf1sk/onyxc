#include "common.h"
#include "amd64.h"

#include "../interm.h"
#include "../segment.h"

#define AMD64_BLOCK_SIZE 512
#define AMD64_BLOCK_MASK (AMD64_BLOCK_SIZE-1)

usz emit(amd64_ctx_t* cx, amd64_instr_t instr) {
	LT_ASSERT(cx->curr_func != -1);

	seg_ent_t* ent = &cx->cs[cx->curr_func];

	if (!(ent->size & AMD64_BLOCK_MASK))
		ent->data = realloc(ent->data, (ent->size + AMD64_BLOCK_SIZE) * sizeof(amd64_instr_t));

	((amd64_instr_t*)ent->data)[ent->size] = instr;
	return ent->size++;
}

void gen_modrm(amd64_ctx_t* cx, ival_t* iv, u8* mod, u8* reg_rm) {
	u8 rm;

	switch (iv->stype) {
	case IVAL_REG: rm = iv->reg & 0b1111; *mod = MOD_REG; break;
	case IVAL_IMM: LT_ASSERT_NOT_REACHED(); break;
	case IVAL_CSO: LT_ASSERT_NOT_REACHED(); break;
	case IVAL_DSO: LT_ASSERT_NOT_REACHED(); break;

	case IVAL_REG | IVAL_REF: rm = iv->reg & 0b1111; *mod = MOD_DREG; break;
	case IVAL_IMM | IVAL_REF: LT_ASSERT_NOT_REACHED(); break;
	case IVAL_CSO | IVAL_REF: LT_ASSERT_NOT_REACHED(); break;
	case IVAL_DSO | IVAL_REF: LT_ASSERT_NOT_REACHED(); break;
	}

	*reg_rm |= rm << 4;
}


