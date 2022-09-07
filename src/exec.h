#ifndef EXEC_H
#define EXEC_H 1

#include "fwd.h"

#include <lt/lt.h>

typedef
struct exec_ctx {
	icode_t* ip;
} exec_ctx_t;

void icode_exec(exec_ctx_t* cx);

#endif
