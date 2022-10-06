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

#define SEG_CODE 0
#define SEG_DATA 2

typedef
struct seg_ent {
	u64 stype;
	lstr_t name;
	type_t* type;
	fwd_ref_t* ref;
	usz size;
	void* data;
	void* jit_data;

	// icode
	symtab_t* label_symtab;
	usz regs;
	usz lbls;
	usz icode_count;
	void* icode_data;

	// mcode
	u64 load_at;
	amd64_lbl_t* lbl;
	usz mcode_count;
	void* mcode_data;
} seg_ent_t;

typedef
struct segtab {
	usz count;
	seg_ent_t* seg;
} segtab_t;


#define SEGMENT_INIT(stype, name, type, ...) { (stype), (name), (type), __VA_ARGS__ }
#define SEGMENT(stype, name, type, ...) (seg_ent_t)SEGMENT_INIT(stype, name, type, __VA_ARGS__)

#define CODESEG(name, type) SEGMENT(SEG_CODE, name, type)
#define DATASEG(name, size_, data_) SEGMENT(SEG_DATA, name, NULL, .size=(size_), .data=(data_))

usz add_segment(segtab_t* tab, seg_ent_t* seg);

static LT_INLINE
usz new_segment(segtab_t* tab, seg_ent_t seg) {
	return add_segment(tab, &seg);
}

void print_segment(segtab_t* tab, usz i);

#endif
