#include "stmt_ast.h"

lstr_t stmt_type_str(stmt_stype_t stype) {
	switch (stype) {
#define STMT_OP(x) case STMT_##x: return CLSTR(#x);
	FOR_EACH_STMT()
#undef STMT_OP
	default:
		return CLSTR("INVALID_STATEMENT");
	}
}

