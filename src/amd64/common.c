#include "common.h"
#include "amd64.h"

#include "../interm.h"
#include "../segment.h"

#define AMD64_BLOCK_SIZE 512
#define AMD64_BLOCK_MASK (AMD64_BLOCK_SIZE-1)

usz emit(amd64_ctx_t* cx, amd64_instr_t instr) {
	LT_ASSERT(cx->curr_func != -1);

	seg_ent_t* ent = &cx->seg[cx->curr_func];

	if (!(ent->size & AMD64_BLOCK_MASK))
		ent->data = realloc(ent->data, (ent->size + AMD64_BLOCK_SIZE) * sizeof(amd64_instr_t));

	((amd64_instr_t*)ent->data)[ent->size] = instr;
	return ent->size++;
}

#define SEGMENT_BLOCK_SIZE 512
#define SEGMENT_BLOCK_MASK (SEGMENT_BLOCK_SIZE-1)

usz new_code_seg(amd64_ctx_t* cx, type_t* type) {
	if (!(cx->seg_count & SEGMENT_BLOCK_MASK))
		cx->seg = realloc(cx->seg, (cx->seg_count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	memset(&cx->seg[cx->seg_count], 0, sizeof(seg_ent_t));
	cx->seg[cx->seg_count].type = type;
	cx->seg[cx->seg_count].stype = SEG_MCODE;
	return cx->seg_count++;
}

