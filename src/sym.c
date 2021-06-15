#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "sym.h"

#define ALLOC_CHUNK_SIZE 1024

usz add_symbol(Symbols* tab, SymbolType sym_stype, LenStr name, TypeHandle type) {
	usz type_count = tab->sym_count;
	LenStr* name_arr = tab->names;
	Symbol* sym_arr = tab->syms;

	if (type_count % ALLOC_CHUNK_SIZE == 0) {
		usz new_count = type_count + ALLOC_CHUNK_SIZE;
		name_arr = realloc(tab->names, new_count * sizeof(LenStr));
		sym_arr = realloc(tab->syms, new_count * sizeof(Symbol));
		assert(name_arr);
		assert(sym_arr);
	}

	name_arr[type_count] = name;
    sym_arr[type_count] = (Symbol) { sym_stype, -1, type };

	++tab->sym_count;
	tab->names = name_arr;
	tab->syms = sym_arr;

	return type_count;
}

SymbolHandle find_symbol(Symbols* tab, const LenStr name) {
	Symbols* curr_tab = tab;

	while (curr_tab) {
		usz sym_count = curr_tab->sym_count;
		LenStr* name_arr = curr_tab->names;

		for (usz i = 0; i < sym_count; ++i) {
			LenStr sym_name = name_arr[i];
			if (name.len == sym_name.len && strncmp(name.str, sym_name.str, name.len) == 0)
				return make_sym_handle(curr_tab, i);
		}

		curr_tab = curr_tab->next;
	}

	return INVALID_SYMBOL;
}

TypeHandle find_type(Symbols* tab, const LenStr name) {
	SymbolHandle sym = find_symbol(tab, name);
	if (!sym_handle_valid(sym) || sym.tab->syms[sym.offs].stype != SM_TYPE)
		return INVALID_TYPE;
	return sym.tab->syms[sym.offs].type;
}

void free_sym_tab(Symbols* tab) {
	if (tab->names)
		free(tab->names);
	if (tab->syms)
		free(tab->syms);
}
