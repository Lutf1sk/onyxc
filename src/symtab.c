#include "symtab.h"

#include <lt/align.h>
#include <lt/str.h>
#include <lt/mem.h>

static
u8 hash_lstr_start(lstr_t str) {
	return (str.len & 3) | ((*str.str & 0x3F) << 2);
}

static
u8 hash_lstr_end(lstr_t str) {
	return (str.len & 3) | ((str.str[str.len] & 0x3F) << 2);
}

static
sym_t* subtab_find(sympool_t* subtab, lstr_t name) {
	u8 index = hash_lstr_end(name);
	sympool_t pool = subtab[index];

	for (usz i = 0; i < pool.count; ++i) {
		if (lt_lstr_eq(pool.keys[i], name))
			return pool.values[i];
	}
	return NULL;
}

sym_t* symtab_find(symtab_t* tab, lstr_t name) {
	u8 index = hash_lstr_start(name);
	sympool_t pool = tab->pools[index];

	if (pool.count == SYMPOOL_SUBTAB)
		return subtab_find(pool.subtab, name);

	for (usz i = 0; i < pool.count; ++i) {
		if (lt_lstr_eq(pool.keys[i], name))
			return pool.values[i];
	}

	if (tab->parent)
		return symtab_find(tab->parent, name);
	return NULL;
}

#define ALLOC_COUNT 16

#include <lt/io.h>

static
void pool_append(sympool_t* pool, lstr_t name, sym_t* sym) {
	usz last_count = pool->count++;

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

	if (pool->count == SYMPOOL_SUBTAB) {
		index = hash_lstr_end(name);
		pool_append(&pool->subtab[index], name, sym);
		return;
	}

	pool_append(pool, name, sym);
}

