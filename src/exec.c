#include "exec.h"

#include "segment.h"
#include "interm.h"

#include <lt/mem.h>
#include <lt/io.h>
#include <lt/align.h>

u64 syscall(u64, ...);

static
i64 sign_extend(usz from, i64 v) {
	switch (from) {
	case 1: return (i8)v;
	case 2: return (i16)v;
	case 4: return (i32)v;
	case 8: return (i64)v;
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

void icode_exec(exec_ctx_t* cx) {
// 	usz arg_stack = 0;

	icode_t* ip = cx->ip;
	for (;;) {
		switch (ip->op) {
// 		case IR_SYSCALL: {
// 			usz count = ip->regs[0], code = -1;
// 			u64 a1, a2, a3, a4;
// 			switch (count) {
// 			case 1:
// 				a1 = pop64(cx);
// 				code = syscall(a1);
// 				break;
// 			case 2:
// 				a2 = pop64(cx), a1 = pop64(cx);
// 				code = syscall(a1, a2);
// 				break;
// 			case 3:
// 				a3 = pop64(cx), a2 = pop64(cx), a1 = pop64(cx);
// 				code = syscall(a1, a2, a3);
// 				break;
// 			case 4:
// 				a4 = pop64(cx), a3 = pop64(cx), a2 = pop64(cx), a1 = pop64(cx);
// 				code = syscall(a1, a2, a3, a4);
// 				break;

// 			default:
// 				LT_ASSERT_NOT_REACHED();
// 			}
// 			cx->regs[cx->reg_offs + ip->dst] = code;
// 		}	break;

		default:
			lt_ferrf("Unhandled operation '%S'\n", icode_op_str(ip->op));
		}
		++ip;
	}
}

