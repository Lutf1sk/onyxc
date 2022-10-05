#include "lbl.h"
#include "common.h"

#include "../segment.h"

#include <lt/io.h>

amd64_lbl_t* new_lbl(lt_arena_t* arena, amd64_lbl_t** lbl, usz lbl_i) {
	amd64_lbl_t** it = lbl;

	while (*it) {
		if ((*it)->i == lbl_i)
			return *it;
		if ((*it)->i > lbl_i)
			break;

		it = &(*it)->next;
	}

	amd64_lbl_t* new = lt_amalloc(arena, sizeof(amd64_lbl_t));
	new->i = lbl_i;
	new->m_i = 0;
	new->next = *it;
	*it = new;
	return new;
}

amd64_lbl_t* find_lbl(amd64_lbl_t* it, usz lbl_i) {
	while (it) {
		if (it->i == lbl_i)
			return it;
		it = it->next;
	}

	return NULL;
}

