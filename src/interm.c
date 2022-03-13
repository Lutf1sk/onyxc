#include "interm.h"

lstr_t icode_op_str(u16 op) {
	switch (op) {
#define ICODE_OP(x) case IR_##x: return CLSTR(#x);
	FOR_EACH_ICODE()
#undef ICODE_OP
	default:
		return CLSTR("INVALID_ICODE");
	}
}

lstr_t icode_size_str(u64 size) {
	switch (size) {
	case ISZ_8: return CLSTR("i8");
	case ISZ_16: return CLSTR("i16");
	case ISZ_32: return CLSTR("i32");
	case ISZ_64: return CLSTR("i64");
	default:
		return CLSTR("v");
	}
}

