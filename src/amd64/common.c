#include "common.h"
#include "amd64.h"
#include "lbl.h"

#include "../interm.h"
#include "../segment.h"

#define AMD64_BLOCK_SIZE 512
#define AMD64_BLOCK_MASK (AMD64_BLOCK_SIZE-1)

void ireg_copy(amd64_ctx_t* cx, u32 dst, u32 src) {
	amd64_ireg_t* dst_ireg = &cx->reg_map[dst];
	amd64_ireg_t* src_ireg = &cx->reg_map[src];

	if ((src_ireg->type & ~IREG_REF) != IREG_REG) {
		*dst_ireg = *src_ireg;
		return;
	}

	if (cx->reg_lifetimes[dst] > cx->reg_lifetimes[src])
		cx->reg_lifetimes[src] = 0;
	else
		cx->reg_lifetimes[dst] = 0;
	*dst_ireg = *src_ireg;
}

b8 ireg_reg_pure(amd64_ireg_t* ireg) {
	return ireg->type == IREG_REG && !ireg->disp;
}

b8 ireg_reg_displaced(amd64_ireg_t* ireg) {
	return ireg->type == IREG_REG && ireg->disp;
}

usz emit(amd64_ctx_t* cx, amd64_instr_t mi) {
	LT_ASSERT(cx->curr_func != -1);

	seg_ent_t* ent = &cx->seg[cx->curr_func];
	if (!(ent->size & AMD64_BLOCK_MASK))
		ent->data = realloc(ent->data, (ent->size + AMD64_BLOCK_SIZE) * sizeof(amd64_instr_t));

	((amd64_instr_t*)ent->data)[ent->size] = mi;

	if ((mi.flags[0] & MI_LBL) || (mi.flags[1] & MI_LBL))
		add_lbl_ref(cx, find_lbl(cx, mi.imm), ent->size);

	return ent->size++;
}

#define SEGMENT_BLOCK_SIZE 512
#define SEGMENT_BLOCK_MASK (SEGMENT_BLOCK_SIZE-1)

usz new_mcode_seg(amd64_ctx_t* cx, type_t* type, lstr_t name, u32 origin) {
	if (!(cx->seg_count & SEGMENT_BLOCK_MASK))
		cx->seg = realloc(cx->seg, (cx->seg_count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	memset(&cx->seg[cx->seg_count], 0, sizeof(seg_ent_t));
	cx->seg[cx->seg_count].type = type;
	cx->seg[cx->seg_count].stype = SEG_MCODE;
	cx->seg[cx->seg_count].name = name;
	cx->seg[cx->seg_count].origin = origin;
	return cx->seg_count++;
}

