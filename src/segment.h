#ifndef SEGMENT_H
#define SEGMENT_H 1

#include "type.h"

#include <lt/lt.h>

typedef
struct fwd_ref {
	usz offs;
	usz seg;
	usz size;
	struct fwd_ref* next;
} fwd_ref_t;

#define SEG_ICODE 0
#define SEG_MCODE 1
#define SEG_DATA 2

typedef
struct seg_ent {
	u64 stype;
	lstr_t name;
	usz size;
	void* data;
	type_t* type;
	usz regs;
	usz lbls;
	u64 load_at;
	u32 origin;
	amd64_lbl_t* lbl;
	fwd_ref_t* ref;
	symtab_t* label_symtab;
} seg_ent_t;

#define SEG_ENT_INIT(stype, name, size, data) { (stype), (name), (size), (data), NULL, 0, 0, 0, 0, NULL, NULL, NULL }
#define SEG_ENT(stype, name, size, data) ((seg_ent_t)SEG_ENT_INIT(stype, name, size, data))

#endif
