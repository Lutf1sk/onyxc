#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "tk.h"
#include "ast.h"
#include "parse.h"

#define ALLOC_CHUNK_SIZE 32

const char* expr_type_str(ExpressionType type) {
	switch (type) {
#define EXPR_OP(x) case EXPR_##x: return #x;
		FOR_EACH_EXPR()
#undef EXPR_OP
		default:
			return "INVALID";
	}
}

void print_expr(ParseCtx* cx, Expression* expr, usz indent) {
	usz child_count = expr->child_count;
	Expression* expr_it = expr->children;

	for (usz i = 0; i + 1 < indent; ++i)
		printf("|  ");
	if (indent)
		printf("|-");

	printf("%s ", expr_type_str(expr->type));

	switch (expr->type) {
		case EXPR_INTEGER: printf("%lu", expr->lit_uint); break;
		case EXPR_FLOAT: printf("%fl", expr->lit_float); break;
		default:
			break;
	}

	putchar('\n');

	for (usz i = 0; i < child_count; ++i)
		print_expr(cx, expr_it++, indent + 1);
}

void free_expr_children(Expression* expr) {
	usz child_count = expr->child_count;
	Expression* expr_it = expr->children;

	for (usz i = 0; i < child_count; ++i)
		free_expr_children(expr_it++);

	if (child_count)
		free(expr->children);
}

void add_expr_child(Expression* parent, const Expression* child) {
	if (parent->child_count % ALLOC_CHUNK_SIZE == 0) {
		parent->children = realloc(parent->children, (parent->child_count + ALLOC_CHUNK_SIZE) * sizeof(Expression));
		assert(parent->children);
	}
	parent->children[parent->child_count++] = *child;
}

Expression make_expr(ExpressionType type) {
	Expression expr = {};
	memset(&expr, 0, sizeof(Expression));
	expr.type = type;
	return expr;
}

Expression FLATTEN make_un_expr(ExpressionType type, Expression* expr) {
	Expression nexpr = make_expr(type);
	add_expr_child(&nexpr, expr);
	return nexpr;
}

Expression FLATTEN make_bin_expr(ExpressionType type, Expression* left, Expression* right) {
	Expression nexpr = make_expr(type);
	add_expr_child(&nexpr, left);
	add_expr_child(&nexpr, right);
	return nexpr;
}
