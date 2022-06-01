#include "lbl.h"
#include "common.h"

#include "../segment.h"

#include <lt/io.h>

amd64_lbl_t* new_lbl(amd64_ctx_t* cx, usz lbl_i) {
	amd64_lbl_t** it = &cx->lbl;

	while (*it) {
		if ((*it)->i == lbl_i)
			return *it;
		if ((*it)->i > lbl_i)
			break;

		it = &(*it)->next;
	}

	amd64_lbl_t* new = lt_arena_reserve(cx->arena, sizeof(amd64_lbl_t));
	new->i = lbl_i;
	new->m_i = 0;
	new->next = *it;
	*it = new;
	return new;
}

amd64_lbl_t* find_lbl(amd64_ctx_t* cx, usz lbl_i) {
	amd64_lbl_t* it = cx->lbl;

	while (it) {
		if (it->i == lbl_i)
			return it;
		it = it->next;
	}

	LT_ASSERT_NOT_REACHED();
	return NULL;
}

void add_lbl_ref(amd64_ctx_t* cx, amd64_lbl_t* lbl, i32 i) {
	seg_ent_t* seg = &cx->seg[cx->curr_func];
	if (lbl->m_i) {
		if (i < 0)
			((amd64_instr_t*)seg->data)[-i].mrm.disp = lbl->m_i;
		else
			((amd64_instr_t*)seg->data)[i].imm.index = lbl->m_i;
	}
	else {
		if (!lbl->ref_count)
			lbl->refs = lt_arena_reserve(cx->arena, sizeof(u32) * 16); // !! Terrible, horrible stuff. Fix this
		lbl->refs[lbl->ref_count++] = i;
	}
}

void resolve_lbls(amd64_ctx_t* cx, u32 i) {
	amd64_lbl_t* lbl = cx->lbl_it;
	if (!lbl || lbl->i != i)
		return;

	seg_ent_t* seg = &cx->seg[cx->curr_func];
	lbl->m_i = seg->size;

	for (usz i = 0; i < lbl->ref_count; ++i) {
		i32 j = lbl->refs[i];
		if (j < 0)
			((amd64_instr_t*)seg->data)[-j].mrm.disp = lbl->m_i;
		else
			((amd64_instr_t*)seg->data)[j].imm.index = lbl->m_i;
	}

	cx->lbl_it = lbl->next;
}

