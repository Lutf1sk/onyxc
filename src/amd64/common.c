#include "common.h"
#include "amd64.h"

#include "../interm.h"
#include "../segment.h"

#define AMD64_BLOCK_SIZE 512
#define AMD64_BLOCK_MASK (AMD64_BLOCK_SIZE-1)

b8 is_ref(u8 stype) {
// 	switch (stype) {
// 	case MVAL_REG: case MVAL_IMM: case MVAL_LBL:
// 		return 1;
// 	default:
// 		return 0;
// 	}
	return 0;
}

amd64_mval_t ival_convert(amd64_ctx_t* cx, ival_t* ival) {
	amd64_mval_t mval = {};
// 	memset(&mval, 0, sizeof(mval));
// 
// 	switch (ival->stype) {
// 	case IVAL_REG:
// 		mval.stype = MVAL_REG;
// 		break;
// 
// 	case IVAL_IMM:
// 		mval.stype = MVAL_IMM;
// 		mval.disp = ival->uint_val;
// 		break;
// 
// 	case IVAL_CSO:
// 		mval.stype = MVAL_LBL;
// 		mval.disp = ival->disp;
// 		break;
// 
// 	case IVAL_DSO:
// 		mval.stype = MVAL_LBL;
// 		mval.disp = ival->disp;
// 		break;
// 
// 	case IVAL_REG | IVAL_REF:
// 		break;
// 	case IVAL_IMM | IVAL_REF:
// 		break;
// 	case IVAL_CSO | IVAL_REF:
// 		break;
// 	case IVAL_DSO | IVAL_REF:
// 		break;
// 	}
// 
// 	LT_ASSERT(mval.stype != MVAL_INVAL);
	return mval;
}

usz emit(amd64_ctx_t* cx, amd64_instr_t instr) {
// 	LT_ASSERT(cx->curr_func != -1);
// 
// 	seg_ent_t* ent = &cx->cs[cx->curr_func];
// 
// 	if (!(ent->size & AMD64_BLOCK_MASK))
// 		ent->data = realloc(ent->data, (ent->size + AMD64_BLOCK_SIZE) * sizeof(amd64_instr_t));
// 
// 	((amd64_instr_t*)ent->data)[ent->size] = instr;
// 	return ent->size++;
	return 0;
}

void gen_op3(amd64_ctx_t* cx, u8 op, amd64_mval_t* dst, amd64_mval_t* src1, amd64_mval_t* src2) {
// 	amd64_instr_t instr;
// 	instr.op = op;
// 	instr.var = 0;
// 
// 	emit(cx, instr);
}

