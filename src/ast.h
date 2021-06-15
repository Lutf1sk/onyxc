#ifndef AST_H
#define AST_H

#include <stddef.h>

#include "fwd.h"
#include "common.h"
#include "sym.h"

#define FOR_EACH_EXPR() \
	EXPR_OP(INVALID) \
	EXPR_OP(ADD) \
	EXPR_OP(SUBTRACT) \
	EXPR_OP(MULTIPLY) \
	EXPR_OP(DIVIDE) \
	EXPR_OP(INTEGER) \
	EXPR_OP(FLOAT) \
    EXPR_OP(LABEL)

typedef
enum ExpressionType {
#define EXPR_OP(x) EXPR_##x,
	FOR_EACH_EXPR()
#undef EXPR_OP
} ExpressionType;

typedef
struct Expression {
	ExpressionType type;

	usz child_count;
	Expression* children;

    TypeHandle datatype;

	union {
		long lit_int;
		unsigned long lit_uint;
		double lit_float;
        SymbolHandle sym_hnd;
	};

	b8 is_comp_time_const;
} Expression;

const char* expr_type_str(ExpressionType type);

void print_expr(ParseCtx* cx, Expression* expr, usz indent);
void free_expr_children(Expression* expr);

void add_expr_child(Expression* parent, const Expression* child);
Expression make_expr(ExpressionType type);
Expression make_un_expr(ExpressionType type, Expression* expr);
Expression make_bin_expr(ExpressionType type, Expression* left, Expression* right);

#endif // AST_H
