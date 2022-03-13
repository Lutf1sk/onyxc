#ifndef SEGMENT_H
#define SEGMENT_H 1

#include "type.h"

#include <lt/lt.h>

#define SEG_CODE 0
#define SEG_DATA 1

typedef
struct seg_ent {
	u64 stype;
	lstr_t name;
	usz size;
	void* data;
	type_t* type;
	usz regs;
} seg_ent_t;

#define SEG_ENT_INIT(stype, name, size, data) { (stype), (name), (size), (data) }
#define SEG_ENT(stype, name, size, data) ((seg_ent_t)SEG_ENT_INIT(stype, name, size, data))

#endif
