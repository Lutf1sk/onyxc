#include "hashtab.h"

#include <lt/align.h>
#include <lt/mem.h>

#define HASHTAB_ALLOC_COUNT 16

#include <lt/io.h>

void hashtab_insert(hashtab_t* htab, u32 hash, void* val) {
	u32 idx = hash & HASHTAB_MASK;
	usz count = htab->counts[idx];
	void** vals = htab->values[idx];

	if (!count) {
		htab->values[idx] = val;
		++htab->counts[idx];
		return;
	}
	else if (count == 1) {
		void* old_val = vals;
		vals = malloc(HASHTAB_ALLOC_COUNT * sizeof(void*));
		LT_ASSERT(vals);
		vals[0] = old_val;

		htab->values[idx] = vals;
	}
	else if (lt_is_pow2(count)) {
		vals = realloc(vals, (count << 1) * sizeof(void*));
		LT_ASSERT(vals);

		htab->values[idx] = vals;
	}

	vals[htab->counts[idx]++] = val;
}

