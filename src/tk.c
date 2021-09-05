#include "tk.h"

lstr_t tk_type_str(tk_stype_t stype) {
	switch (stype) {
#define TK_OP(x) case TK_##x: return CLSTR(#x);
	FOR_EACH_TK()
#undef TK_OP
	default:
		return CLSTR("INVALID_TOKEN");
	}
}

