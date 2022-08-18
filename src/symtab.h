#ifndef SYMTAB_H
#define SYMTAB_H 1

#include <lt/lt.h>
#include <lt/fwd.h>
#include <lt/mem.h>

#include "fwd.h"

#include "interm.h"

#define SYMPOOL_SUBTAB ((u32)-1)

typedef
enum sym_stype {
	SYM_NONE	= 0x00,
	SYM_VAR		= 0x01,
	SYM_TYPE	= 0x02,
	SYM_LABEL	= 0x03,
	SYM_NSPACE	= 0x04,
} sym_stype_t;

typedef
enum sym_flags {
	SYMFL_CONST			= 0x01,
	SYMFL_GLOBAL		= 0x02,
	SYMFL_ACCESSED		= 0x04,
	SYMFL_REFERENCED	= 0x08,
	SYMFL_ARG			= 0x10,
} sym_flags_t;

typedef
struct sym {
	sym_stype_t stype;
	sym_flags_t flags;
	lstr_t name;
	type_t* type;
	expr_t* expr;
	ival_t val;
	usz lbl;
} sym_t;

#define SYM_INIT(stype, name) { (stype), 0, (name), NULL, NULL, {}, 0 }
#define SYM(stype, name) ((sym_t)SYM_INIT(stype, name))

typedef
struct sympool {
	lstr_t* keys;
	union {
		sym_t** values;
		sympool_t* subtab;
	};
} sympool_t;

typedef
struct symtab {
	usz count;
	symtab_t* parent;
	u32 counts[256];
	sympool_t pools[256];
} symtab_t;

sym_t* symtab_find(symtab_t* tab, lstr_t name);
b8 symtab_definable(symtab_t* tab, lstr_t name);
void symtab_insert(symtab_t* tab, lstr_t name, sym_t* sym);

symtab_t* symtab_create(lt_arena_t* arena);

#endif
