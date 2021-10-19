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

ival_t expr_eval_const(parse_ctx_t* cx, expr_t* expr, tk_t* tk) {
	// TODO: Evaluate constant expressions properly

	switch (expr->stype) {
	case EXPR_NEGATE:
		return IVAL(ISZ_64, IVAL_IMM, .uint_val = -expr_eval_const(cx, expr->child_1, tk).uint_val);

	case EXPR_INTEGER:
		return IVAL(ISZ_64, IVAL_IMM, .uint_val = expr->uint_val);

	default:
		ferr("expression is not constant", cx->lex, *tk);
	}
}

