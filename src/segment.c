#include "segment.h"

#define SEGMENT_BLOCK_SIZE 16
#define SEGMENT_BLOCK_MASK (SEGMENT_BLOCK_SIZE-1)

usz add_segment(segtab_t* tab, seg_ent_t* seg) {
	if (!(tab->count & SEGMENT_BLOCK_MASK))
		tab->seg = realloc(tab->seg, (tab->count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	tab->seg[tab->count] = *seg;
	return tab->count++;
}

