#ifndef STMT_AST_H
#define STMT_AST_H 1

#include <lt/lt.h>
#include <lt/mem.h>

#include "fwd.h"

#define FOR_EACH_STMT() \
	STMT_OP(IF) \
	STMT_OP(WHILE) \
	STMT_OP(FOR) \
	STMT_OP(LET) \
	STMT_OP(DEF) \
	STMT_OP(RETURN) \
	STMT_OP(BREAK) \
	STMT_OP(CONTINUE) \
	STMT_OP(COMPOUND) \
	STMT_OP(EXPR)

typedef enum stmt_stype {
#define STMT_OP(x) STMT_##x,
	FOR_EACH_STMT()
#undef STMT_OP
} stmt_stype_t;

lstr_t stmt_type_str(stmt_stype_t stype);

typedef struct stmt {
	stmt_stype_t stype;
	stmt_t* child, *next;
	expr_t* expr;
	type_t* type;
	lstr_t identifier;
} stmt_t;

static LT_INLINE
stmt_t stmt_make(stmt_stype_t stype) {
	stmt_t stmt;
	memset(&stmt, 0, sizeof(stmt));
	stmt.stype = stype;
	return stmt;
}

#endif