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
	STMT_OP(EXPR) \
	STMT_OP(SYSCALL)

typedef enum stmt_stype {
#define STMT_OP(x) STMT_##x,
	FOR_EACH_STMT()
#undef STMT_OP
} stmt_stype_t;

lstr_t stmt_type_str(stmt_stype_t stype);

typedef struct stmt {
	stmt_stype_t stype;
	stmt_t* child, *next, *child_2;
	expr_t* expr;
	type_t* type;
	sym_t* sym;
} stmt_t;

#define STMT_INIT(stype) { (stype), NULL, NULL, NULL, NULL, NULL, NULL }
#define STMT(stype) ((stmt_t)STMT_INIT(stype))

#endif
