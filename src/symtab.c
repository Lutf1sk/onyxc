#include "symtab.h"

#include <lt/align.h>
#include <lt/str.h>
#include <lt/mem.h>

static
u8 hash_lstr_start(lstr_t str) {
	return (str.len & 3) | ((*str.str & 0x3F) << 2);
}

// static
// u8 hash_lstr_end(lstr_t str) {
// 	return (str.len & 3) | ((str.str[str.len] & 0x3F) << 2);
// }

sym_t* symtab_find(symtab_t* tab, lstr_t name) {
	u8 index = hash_lstr_start(name);
	sympool_t pool = tab->pools[index];
	usz count = tab->counts[index];

	for (usz i = 0; i < count; ++i) {
		if (lt_lstr_eq(pool.keys[i], name))
			return pool.values[i];
	}

	if (tab->parent)
		return symtab_find(tab->parent, name);
	return NULL;
}

b8 symtab_definable(symtab_t* tab, lstr_t name) {
	u8 index = hash_lstr_start(name);
	sympool_t pool = tab->pools[index];
	usz count = tab->counts[index];

	for (usz i = 0; i < count; ++i) {
		if (lt_lstr_eq(pool.keys[i], name))
			return 0;
	}

	return 1;
}

#define ALLOC_COUNT 16

#include <lt/io.h>

static
void pool_append(sympool_t* pool, u32* count, lstr_t name, sym_t* sym) {
	usz last_count = (*count)++;

	if (!last_count) {
		pool->keys = malloc(ALLOC_COUNT * (sizeof(lstr_t) + sizeof(sym_t*)));
		pool->values = (sym_t**)(pool->keys + ALLOC_COUNT);
	}

 	pool->keys[last_count] = name;
 	pool->values[last_count] = sym;
}

void symtab_insert(symtab_t* tab, lstr_t name, sym_t* sym) {
	u8 index = hash_lstr_start(name);
	sympool_t* pool = &tab->pools[index];

// 	if (tab->counts[index] == SYMPOOL_SUBTAB) {
// 		index = hash_lstr_end(name);
// 		pool_append(&pool->subtab[index], &tab->counts[index], name, sym);
// 		return;
// 	}

	pool_append(pool, &tab->counts[index], name, sym);
}

