#ifndef EXEC_H
#define EXEC_H 1

#include "fwd.h"

#include <lt/lt.h>

typedef
struct exec_ctx {
	u8* sp;
	u8* bp;
	icode_t* ip;

	u64 regs[32];
	u64 args[32];
	u64 retval;

	seg_ent_t* cs;
	seg_ent_t* ds;
} exec_ctx_t;

u64 icode_exec(exec_ctx_t* cx);

#endif
