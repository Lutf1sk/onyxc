#ifndef TYPE_H
#define TYPE_H

#include "fwd.h"
#include "common.h"

#define INVALID_TYPE ((TypeHandle) { NULL, 0 })

typedef
enum TypeSType {
	TP_INVALID,

	TP_U64, TP_U32, TP_U16, TP_U8,
	TP_I64, TP_I32, TP_I16, TP_I8,
	TP_F64, TP_F32,

	TP_STRUCT,
	TP_ARRAY,
} TypeSType;

typedef
struct TypeHandle {
	Types* tab;
	usz offs;
} TypeHandle;

typedef
struct TypeInfo {
	TypeSType stype;

	TypeHandle base_type;

	usz child_count;
	TypeHandle* children;
} TypeInfo;


typedef
struct Types {
	usz type_count;

	LenStr* name_arr;
	TypeInfo* info_arr;
} Types;


static inline INLINE
Types make_type_tab() {
	return (Types) { 0, NULL, NULL };
}

void free_type_tab(Types* tab);

usz add_type(Types* tab, const LenStr name, TypeInfo* info);
usz add_primitive(Types* tab, const LenStr name, TypeSType type);

static inline INLINE
TypeHandle make_type_handle(Types* tab, usz offs) {
	return (TypeHandle) { tab, offs };
}

static inline INLINE
b8 type_handle_valid(TypeHandle hnd) {
	return hnd.tab != NULL;
}

#endif // TYPE_H
