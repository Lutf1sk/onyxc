#ifndef SYMTAB_H
#define SYMTAB_H 1

#include <lt/lt.h>
#include <lt/fwd.h>
#include <lt/mem.h>

#include <stdint.h>

#include "fwd.h"

#define SYMPOOL_SUBTAB SIZE_MAX

typedef
enum sym_stype {
	SYM_VAR,
	SYM_TYPE,
} sym_stype_t;

typedef
enum sym_flags {
	SYMFL_CONST = 1,
} sym_flags_t;

typedef
struct sym {
	sym_stype_t stype;
	sym_flags_t flags;
	lstr_t name;
	type_t* type;
	expr_t* expr;
} sym_t;

#define SYM_INIT(stype, name) { (stype), 0, (name), NULL, NULL }
#define SYM(stype, name) ((sym_t)SYM_INIT(stype, name))

typedef
struct sympool {
	usz count;
	lstr_t* keys;
	union {
		sym_t** values;
		sympool_t* subtab;
	};
} sympool_t;

typedef
struct symtab {
	symtab_t* parent;
	sympool_t pools[UINT8_MAX];
} symtab_t;

sym_t* symtab_find(symtab_t* tab, lstr_t name);
void symtab_insert(symtab_t* tab, lstr_t name, sym_t* sym);

#endif
