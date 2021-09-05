#include "type.h"

isz type_to_str(char* out_str, type_t* type) {
	char* it = out_str;

	lstr_t str = NLSTR();

	switch (type->stype) {
	case TP_VOID: str = CLSTR("void"); goto write;

	case TP_I8: str = CLSTR("i8"); goto write;
	case TP_I16: str = CLSTR("i16"); goto write;
	case TP_I32: str = CLSTR("i32"); goto write;
	case TP_I64: str = CLSTR("i64"); goto write;

	case TP_U8: str = CLSTR("u8"); goto write;
	case TP_U16: str = CLSTR("u16"); goto write;
	case TP_U32: str = CLSTR("u32"); goto write;
	case TP_U64: str = CLSTR("u64"); goto write;

	case TP_B8: str = CLSTR("b8"); goto write;

	case TP_F32: str = CLSTR("f32"); goto write;
	case TP_F64: str = CLSTR("f64"); goto write;

	case TP_STRUCT: str = CLSTR("struct"); goto write;

	write:
		memcpy(it, str.str, str.len);
		return (it - out_str) + str.len;

	case TP_ARRAY:
		it += type_to_str(it, type->base); 
		str = CLSTR("[]");
		goto write;

	case TP_ARRAY_VIEW:
		it += type_to_str(it, type->base);
		str = CLSTR("[]");
		goto write;

	case TP_PTR:
		it += type_to_str(it, type->base);
		str = CLSTR("*");
		goto write;

	case TP_FUNC:
		it += type_to_str(it, type->base);
		str = CLSTR("()");
		goto write;

	default:
		return 0;
	}
}

lstr_t type_to_reserved_str(lt_arena_t* arena, type_t* type) {
	char* str_data = lt_arena_reserve(arena, 0);
	lstr_t str = LSTR(str_data, type_to_str(str_data, type));
	lt_arena_reserve(arena, str.len);
	return str;
}

type_t void_def = { TP_VOID, NLSTR(), NULL, NULL, NULL };

type_t u8_def = { TP_U8, NLSTR(), NULL, NULL, NULL };
type_t u16_def = { TP_U16, NLSTR(), NULL, NULL, NULL };
type_t u32_def = { TP_U32, NLSTR(), NULL, NULL, NULL };
type_t u64_def = { TP_U64, NLSTR(), NULL, NULL, NULL };

type_t i8_def = { TP_I8, NLSTR(), NULL, NULL, NULL };
type_t i16_def = { TP_I16, NLSTR(), NULL, NULL, NULL };
type_t i32_def = { TP_I32, NLSTR(), NULL, NULL, NULL };
type_t i64_def = { TP_I64, NLSTR(), NULL, NULL, NULL };

type_t f32_def = { TP_F32, NLSTR(), NULL, NULL, NULL };
type_t f64_def = { TP_F64, NLSTR(), NULL, NULL, NULL };

type_t b8_def = { TP_B8, NLSTR(), NULL, NULL, NULL };

b8 is_int_any_sign(type_t* type) {
	return type->stype >= TP_U8 && type->stype <= TP_I64;
}

b8 is_int(type_t* type) {
	return type->stype >= TP_I8 && type->stype <= TP_I64;
}

b8 is_uint(type_t* type) {
	return type->stype >= TP_U8 && type->stype <= TP_U64;
}

b8 is_float(type_t* type) {
	return type->stype >= TP_F32 && type->stype <= TP_F64;
}

b8 is_bool(type_t* type) {
	return type->stype == TP_B8;
}

