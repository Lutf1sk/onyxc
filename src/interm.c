#include "interm.h"

lstr_t icode_type_str(icode_type_t type) {
	switch (type) {
#define ICODE_OP(x) case IR_##x: return CLSTR(#x);
	FOR_EACH_ICODE()
#undef ICODE_OP
	default:
		return CLSTR("INVALID_ICODE");
	}
}

lstr_t icode_size_str(ival_t val) {
	if (val.stype == IVAL_INVAL)
		return CLSTR(" ");

	switch (val.size) {
	case ISZ_8: return CLSTR("b");
	case ISZ_16: return CLSTR("w");
	case ISZ_32: return CLSTR("d");
	case ISZ_64: return CLSTR("q");
	default:
		return CLSTR("v");
	}
}

