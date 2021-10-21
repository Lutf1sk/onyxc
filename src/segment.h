#ifndef SEGMENT_H
#define SEGMENT_H 1

#include "type.h"

#include <lt/lt.h>

typedef
struct seg_ent {
	lstr_t name;
	usz size;
	void* data;
	type_t* type;
	isz top;
	usz regs;
} seg_ent_t;

#define SEG_ENT_INIT(name, size, data) { (name), (size), (data) }
#define SEG_ENT(name, size, data) ((seg_ent_t)SEG_ENT_INIT(name, size, data))

#endif
