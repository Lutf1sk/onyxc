#ifndef SYM_H
#define SYM_H

#include "fwd.h"
#include "common.h"
#include "type.h"

#define INVALID_SYMBOL ((SymbolHandle) { NULL, 0 })

typedef
struct SymbolHandle {
	Symbols* tab;
	usz offs;
} SymbolHandle;

typedef
enum SymbolType {
	SM_INVALID,

	SM_CONSTANT,
	SM_LOCAL_VAR,
	SM_GLOBAL_VAR,
	SM_TYPE,
} SymbolType;

typedef
struct Symbol {
	SymbolType stype;

	TypeHandle type;
} Symbol;

typedef
struct Symbols {
	usz sym_count;
	Symbol* syms;
	LenStr* names;
} Symbols;

static inline INLINE
Symbols make_sym_tab() {
	return (Symbols) { 0, NULL, NULL };
}

usz add_symbol(Symbols* tab, SymbolType sym_stype, LenStr name, TypeHandle type);

SymbolHandle find_symbol(Symbols* tab, LenStr name);

void free_sym_tab(Symbols* tab);

static inline INLINE
SymbolHandle make_sym_handle(Symbols* tab, usz offs) {
	return (SymbolHandle) { tab, offs };
}

static inline INLINE
b8 sym_handle_valid(SymbolHandle hnd) {
	return hnd.tab != NULL;
}

#endif // SYM_H
