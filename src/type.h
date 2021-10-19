#ifndef TYPE_H
#define TYPE_H 1

#include <lt/lt.h>
#include <lt/mem.h>

#include "fwd.h"

#define TYPE_MAX_CHILDREN 24

typedef enum type_stype {
	TP_PTR,
	TP_ARRAY, TP_ARRAY_VIEW,
	TP_FUNC,
	TP_STRUCT,

	TP_VOID,
	TP_U8, TP_U16, TP_U32, TP_U64,
	TP_I8, TP_I16, TP_I32, TP_I64,
	TP_F32, TP_F64,
	TP_B8,
} type_stype_t;

typedef struct type {
	type_stype_t stype;
	usz child_count;
	type_t** children;
	lstr_t* child_names;
	sym_t** child_syms;
	sym_t* sym;
	struct type* base;
} type_t;

#define TYPE_INIT(stype, base) { (stype), 0, NULL, NULL, NULL, NULL, (base) }
#define TYPE(stype, base) ((type_t)TYPE_INIT(stype, base))

b8 type_eq(type_t* t1, type_t* t2);

void type_add_child(type_t* type, type_t* child, lstr_t name, sym_t* sym);

b8 is_int_any_sign(type_t* type);

b8 is_scalar(type_t* type);
b8 is_int(type_t* type);
b8 is_uint(type_t* type);
b8 is_float(type_t* type);
b8 is_bool(type_t* type);

usz type_bytes(type_t* type);

isz type_to_str(char* out_str, type_t* type);
lstr_t type_to_reserved_str(lt_arena_t* arena, type_t* type);

b8 type_convert_implicit(parse_ctx_t* cx, type_t* type, expr_t** expr);
b8 type_convert_explicit(parse_ctx_t* cx, type_t* type, expr_t** expr);

void type_make_compatible(parse_ctx_t* cx, tk_t* tk, int stype, expr_t** left, expr_t** right);

extern type_t void_def;
extern type_t void_ptr_def;

extern type_t u8_def;
extern type_t u8_ptr_def;
extern type_t u16_def;
extern type_t u32_def;
extern type_t u64_def;

extern type_t i8_def;
extern type_t i16_def;
extern type_t i32_def;
extern type_t i64_def;

extern type_t f32_def;
extern type_t f64_def;

extern type_t b8_def;

#endif
