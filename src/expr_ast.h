#ifndef EXPR_AST_H
#define EXPR_AST_H 1

#include <lt/lt.h>
#include <lt/mem.h>

#include "fwd.h"
#include "interm.h"

#define FOR_EACH_EXPR() \
	EXPR_OP(INTEGER) \
	EXPR_OP(FLOAT) \
	EXPR_OP(STRING) \
	EXPR_OP(SYM) \
	EXPR_OP(LAMBDA) \
	EXPR_OP(ARRAY) \
	EXPR_OP(STRUCT) \
	\
	EXPR_OP(ADD) \
	EXPR_OP(SUBTRACT) \
	EXPR_OP(MULTIPLY) \
	EXPR_OP(DIVIDE) \
	EXPR_OP(MODULO) \
	EXPR_OP(NEGATE) \
	\
	EXPR_OP(ASSIGN) \
	EXPR_OP(DEREFERENCE) \
	EXPR_OP(REFERENCE) \
	\
	EXPR_OP(PFX_INCREMENT) \
	EXPR_OP(PFX_DECREMENT) \
	EXPR_OP(SFX_INCREMENT) \
	EXPR_OP(SFX_DECREMENT) \
	\
	EXPR_OP(BIT_AND) \
	EXPR_OP(BIT_OR) \
	EXPR_OP(BIT_XOR) \
	EXPR_OP(BIT_NOT) \
	EXPR_OP(BIT_SHIFT_LEFT) \
	EXPR_OP(BIT_SHIFT_RIGHT) \
	\
	EXPR_OP(LOGIC_AND) \
	EXPR_OP(LOGIC_OR) \
	EXPR_OP(LOGIC_NOT) \
	EXPR_OP(LESSER) \
	EXPR_OP(GREATER) \
	EXPR_OP(LESSER_OR_EQUAL) \
	EXPR_OP(GREATER_OR_EQUAL) \
	EXPR_OP(EQUAL) \
	EXPR_OP(NOT_EQUAL) \
	\
	EXPR_OP(ADD_ASSIGN) \
	EXPR_OP(SUBTRACT_ASSIGN) \
	EXPR_OP(MULTIPLY_ASSIGN) \
	EXPR_OP(DIVIDE_ASSIGN) \
	EXPR_OP(MODULO_ASSIGN) \
	EXPR_OP(BIT_SHIFT_LEFT_ASSIGN) \
	EXPR_OP(BIT_SHIFT_RIGHT_ASSIGN) \
	EXPR_OP(BIT_AND_ASSIGN) \
	EXPR_OP(BIT_XOR_ASSIGN) \
	EXPR_OP(BIT_OR_ASSIGN) \
	\
	EXPR_OP(MEMBER) \
	EXPR_OP(SUBSCRIPT) \
	EXPR_OP(CALL) \
	EXPR_OP(CONVERT) \
	EXPR_OP(VIEW) \
	EXPR_OP(DATA) \
	EXPR_OP(COUNT) \
	\
	EXPR_OP(SYSCALL)

typedef enum expr_stype {
#define EXPR_OP(x) EXPR_##x,
	FOR_EACH_EXPR()
#undef EXPR_OP
} expr_stype_t;

lstr_t expr_type_str(expr_stype_t stype);
ival_t expr_eval_const(parse_ctx_t* cx, expr_t* expr, tk_t* tk);

typedef struct expr {
	expr_stype_t stype;
	struct expr* child_1;
	struct expr* child_2;
	union {
		f64 float_val;
		u64 uint_val;
		i64 int_val;
		usz member_index;
		sym_t* sym;
		stmt_t* stmt;
		lstr_t str_val;
	};
	type_t* type;
	struct expr* next;
} expr_t;

#define EXPR_INIT(stype, type) { (stype), NULL, NULL, {}, (type), NULL }
#define EXPR(stype, type) ((expr_t)EXPR_INIT(stype, type))

#endif
