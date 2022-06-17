#include <lt/bits.h>

#define HASHTAB_SIZE 256
#define HASHTAB_MASK (HASHTAB_SIZE-1)

typedef
struct hashtab {
	usz counts[HASHTAB_SIZE];
	void** values[HASHTAB_SIZE];
} hashtab_t;

void hashtab_insert(hashtab_t* htab, u32 hash, void* val);

#define HASHTAB_FIND_IMPL(htab, hash, it, cond) { \
	u32 _idx = hash & HASHTAB_MASK; \
	usz _count = (htab)->counts[_idx]; \
	void** _vals = (htab)->values[_idx]; \
	if (_count == 1) { \
		it = (void*)_vals; \
		if cond \
			return (void*)_vals; \
		return NULL; \
	} \
	\
	for (usz _i = 0; _i < _count; ++_i) { \
		void* _val = _vals[_i]; \
		it = _val; \
		if cond \
			return (void*)_val; \
	} \
	\
	return NULL; \
}

static LT_INLINE
u32 hash(void* _mem, usz size) {
	u8* mem = _mem;
	u32 hash = size;

	for (usz i = 0; i < size; ++i)
		hash ^= lt_rotl32(hash, 7) ^ mem[i];

	return hash;
}

