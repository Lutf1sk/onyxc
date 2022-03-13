#include "expr_ast.h"
#include "parse.h"
#include "err.h"

lstr_t expr_type_str(expr_stype_t stype) {
	switch (stype) {
#define EXPR_OP(x) case EXPR_##x: return CLSTR(#x);
	FOR_EACH_EXPR()
#undef EXPR_OP
		default:
			return CLSTR("INVALID_EXPRESSION");
	}
}

