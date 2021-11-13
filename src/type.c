#include "type.h"
#include "symtab.h"

#include <lt/mem.h>
#include <lt/io.h>

b8 type_eq(type_t* t1, type_t* t2) {
	if (t1->stype != t2->stype)
		return 0;

	switch (t1->stype) {
		case TP_FUNC:
			return type_eq(t1->base, t2->base);

		case TP_PTR:
		case TP_ARRAY: case TP_ARRAY_VIEW:
			return type_eq(t1->base, t2->base);

		default:
			return 1;
	}
}

void type_add_child(type_t* type, type_t* child, lstr_t name, sym_t* sym) {
	usz old_count = type->child_count++;

	type_t** children = realloc(type->children, type->child_count * sizeof(type_t*));
	if (!children)
		goto alloc_failed;

	lstr_t* names = realloc(type->child_names, type->child_count * sizeof(lstr_t));
	if (!names)
		goto alloc_failed;

	sym_t** syms = realloc(type->child_syms, type->child_count * sizeof(sym_t*));
	if (!syms)
		goto alloc_failed;

	children[old_count] = child;
	names[old_count] = name;
	syms[old_count] = sym;

	type->children = children;
	type->child_names = names;
	type->child_syms = syms;

	return;
alloc_failed:
	lt_ferr(CLSTR("Memory allocation failed\n"));
}

isz type_to_str(char* out_str, type_t* type) {
	LT_ASSERT(type);

	char* it = out_str;

	lstr_t str = NLSTR();

	if (type->sym) {
		str = type->sym->name;
		goto write;
	}

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
		it += lt_str_printf(it, "[%uz]", type->child_count);
		return it - out_str;

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
		*(it++) = '(';

		for (usz i = 0; i < type->child_count; ++i) {
			if (i)
				*(it++) = ',';

			it += type_to_str(it, type->children[i]);

			lstr_t name = type->child_names[i];
			if (name.len) {
				*(it++) = ' ';
				memcpy(it, name.str, name.len);
				it += name.len;
			}
		}
		str = CLSTR(")");
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

type_t void_def = TYPE_INIT(TP_VOID, NULL);
type_t void_ptr_def = TYPE_INIT(TP_PTR, &void_def);

type_t u8_def = TYPE_INIT(TP_U8, NULL);
type_t u8_ptr_def = TYPE_INIT(TP_PTR, &u8_def);
type_t u16_def = TYPE_INIT(TP_U16, NULL);
type_t u32_def = TYPE_INIT(TP_U32, NULL);
type_t u64_def = TYPE_INIT(TP_U64, NULL);

type_t i8_def = TYPE_INIT(TP_I8, NULL);
type_t i16_def = TYPE_INIT(TP_I16, NULL);
type_t i32_def = TYPE_INIT(TP_I32, NULL);
type_t i64_def = TYPE_INIT(TP_I64, NULL);

type_t f32_def = TYPE_INIT(TP_F32, NULL);
type_t f64_def = TYPE_INIT(TP_F64, NULL);

type_t b8_def = TYPE_INIT(TP_B8, NULL);

b8 is_int_any_sign(type_t* type) {
	return type->stype >= TP_U8 && type->stype <= TP_I64;
}

b8 is_scalar(type_t* type) {
	return is_int_any_sign(type) || is_bool(type) || type->stype == TP_PTR;
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

usz type_bytes(type_t* type) {
	LT_ASSERT(type);

	switch (type->stype) {
	case TP_VOID:
		return 0;

	case TP_U8: case TP_I8: case TP_B8:
		return 1;
	case TP_U16: case TP_I16:
		return 2;
	case TP_F32: case TP_U32: case TP_I32:
		return 4;
	case TP_F64: case TP_U64: case TP_I64: case TP_PTR: case TP_FUNC:
		return 8;

	case TP_ARRAY_VIEW:
		return 16;

	case TP_ARRAY:
		return type_bytes(type->base) * type->child_count;

	case TP_STRUCT: {
		usz size = 0;
		for (usz i = 0; i < type->child_count; ++i)
			size += type_bytes(type->children[i]);
		return size;
	}
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

usz type_align(type_t* type) {
	LT_ASSERT(type);

	switch (type->stype) {
	case TP_VOID:
		return 0;

	case TP_U8: case TP_I8: case TP_B8:
		return 1;
	case TP_U16: case TP_I16:
		return 2;
	case TP_F32: case TP_U32: case TP_I32:
		return 4;
	case TP_F64: case TP_U64: case TP_I64: case TP_PTR: case TP_FUNC:
		return 8;

	case TP_ARRAY_VIEW: return 8;
	case TP_ARRAY: return type_align(type->base);

	case TP_STRUCT: return 8;
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

