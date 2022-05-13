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

static
u64 uval(exec_ctx_t* cx, usz size, u32 reg) {
	switch (size) {
	case 1: return *(u8*)&cx->regs[cx->reg_offs + reg];
	case 2: return *(u16*)&cx->regs[cx->reg_offs + reg];
	case 4: return *(u32*)&cx->regs[cx->reg_offs + reg];
	case 8: return *(u64*)&cx->regs[cx->reg_offs + reg];
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

static
i64 ival(exec_ctx_t* cx, usz size, u32 reg) {
	switch (size) {
	case 1: return *(i8*)&cx->regs[cx->reg_offs + reg];
	case 2: return *(i16*)&cx->regs[cx->reg_offs + reg];
	case 4: return *(i32*)&cx->regs[cx->reg_offs + reg];
	case 8: return *(i64*)&cx->regs[cx->reg_offs + reg];
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

static
u64 load(usz size, usz addr) {
	switch (size) {
	case 1: return *(u8*)addr;
	case 2: return *(u16*)addr;
	case 4: return *(u32*)addr;
	case 8: return *(u64*)addr;
	default:
		LT_ASSERT_NOT_REACHED();
		return 0;
	}
}

static
void store(usz size, usz addr, u64 val) {
	switch (size) {
	case 1: *(u8*)addr = val; break;
	case 2: *(u16*)addr = val; break;
	case 4: *(u32*)addr = val; break;
	case 8: *(u64*)addr = val; break;
	default:
		LT_ASSERT_NOT_REACHED();
	}
}

static
void push64(exec_ctx_t* cx, u64 v) {
	*(u64*)cx->sp = v;
	cx->sp += 8;
}

static
u64 pop64(exec_ctx_t* cx) {
	return *(u64*)(cx->sp -= 8);
}

static
void push(exec_ctx_t* cx, usz size, void* ptr) {
	memcpy(cx->sp, ptr, size);
	cx->sp += size;
}

void icode_exec(exec_ctx_t* cx) {
	usz arg_stack = 0;

	icode_t* ip = cx->ip;
	for (;;) {
		switch (ip->op) {
		case IR_INT: cx->regs[cx->reg_offs + ip->dst] = ip->uint_val; break;
		case IR_FLOAT: cx->regs[cx->reg_offs + ip->dst] = ip->uint_val; break;
		case IR_SEG: cx->regs[cx->reg_offs + ip->dst] = (u64)cx->seg[ip->uint_val].data; break;
		case IR_IPO: cx->regs[cx->reg_offs + ip->dst] = (u64)(ip + ip->int_val); break;

		case IR_LOAD: cx->regs[cx->reg_offs + ip->dst] = load(ip->size, uval(cx, ISZ_64, ip->regs[0])); break;
		case IR_STOR: store(ip->size, uval(cx, ISZ_64, ip->dst), uval(cx, ip->size, ip->regs[0])); break;

		case IR_NEG: cx->regs[cx->reg_offs + ip->dst] = -uval(cx, ip->size, ip->regs[0]); break;

		case IR_INC: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) + 1; break;
		case IR_DEC: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) - 1; break;

		case IR_ADD: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) + uval(cx, ip->size, ip->regs[1]); break;
		case IR_SUB: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) - uval(cx, ip->size, ip->regs[1]); break;

		case IR_IMUL: cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) * ival(cx, ip->size, ip->regs[1]); break;
		case IR_IDIV: cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) / ival(cx, ip->size, ip->regs[1]); break;
		case IR_IREM: cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) % ival(cx, ip->size, ip->regs[1]); break;

		case IR_UMUL: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) * uval(cx, ip->size, ip->regs[1]); break;
		case IR_UDIV: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) / uval(cx, ip->size, ip->regs[1]); break;
		case IR_UREM: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) % uval(cx, ip->size, ip->regs[1]); break;

		case IR_SRESV: {
			u64 size = ip->regs[0];
			u64 align = ip->regs[1];
			if (!size)
				break;
			LT_ASSERT(align);
			usz padding = lt_pad((usz)cx->sp, align);
			cx->regs[cx->reg_offs + ip->dst] = (u64)cx->sp + padding;
			cx->sp += padding + size;
		}	break;

		case IR_COPY: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]); break;
		case IR_MCOPY: memcpy((void*)uval(cx, ISZ_64, ip->dst), (void*)uval(cx, ISZ_64, ip->regs[0]), ip->size); break;

		case IR_TOI16: cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]); break;
		case IR_TOI32: cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]); break;
		case IR_TOI64: cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]); break;

		case IR_TOU16: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]); break;
		case IR_TOU32: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]); break;
		case IR_TOU64: cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]); break;

		case IR_ENTER:
			push64(cx, (u64)cx->bp);
			cx->bp = cx->sp;
			break;

		case IR_GETARG:
			usz arg_size = ip->size;
			memcpy((void*)uval(cx, ISZ_64, ip->dst), (void*)(cx->bp - 16 - arg_size - arg_stack), arg_size);
			arg_stack += /*lt_align_fwd(*/arg_size/*, 8)*/;
			break;

		case IR_SETARG:
// 			cx->sp = (u8*)lt_align_bwd((usz)cx->sp, 8);
			if (ip->size > ISZ_64)
				push(cx, ip->size, (void*)uval(cx, ISZ_64, ip->dst));
			else
				push(cx, ip->size, &cx->regs[cx->reg_offs + ip->dst]);
			break;

		case IR_RET: {
			if (ip->dst && ip->size) {
				if (ip->size > ISZ_64)
					memcpy(cx->ret_ptr, (void*)uval(cx, ISZ_64, ip->dst), ip->size);
				else {
					u64 v = uval(cx, ip->size, ip->dst);
					memcpy(cx->ret_ptr, &v, ip->size);
				}
			}
			cx->sp = cx->bp;
			cx->bp = (u8*)pop64(cx);
			cx->ip = (icode_t*)pop64(cx);
			return;
		}

		case IR_CJMPZ:
			if (!uval(cx, ip->size, ip->regs[0])) {
				ip = (icode_t*)uval(cx, ISZ_64, ip->dst);
				continue;
			}
			break;

		case IR_CJMPNZ:
			if (uval(cx, ip->size, ip->regs[0])) {
				ip = (icode_t*)uval(cx, ISZ_64, ip->dst);
				continue;
			}
			break;

		case IR_CJMPA:
			if (uval(cx, ip->size, ip->regs[0]) > uval(cx, ip->size, ip->regs[1])) {
				ip = (icode_t*)uval(cx, ISZ_64, ip->dst);
				continue;
			}
			break;

		case IR_CJMPAE:
			if (uval(cx, ip->size, ip->regs[0]) >= uval(cx, ip->size, ip->regs[1])) {
				ip = (icode_t*)uval(cx, ISZ_64, ip->dst);
				continue;
			}
			break;

		case IR_CSETZ:	cx->regs[cx->reg_offs + ip->dst] = !uval(cx, ip->size, ip->regs[0]); break;
		case IR_CSETNZ:	cx->regs[cx->reg_offs + ip->dst] = !!uval(cx, ip->size, ip->regs[0]); break;

		case IR_CSETL:	cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) <	ival(cx, ip->size, ip->regs[1]); break;
		case IR_CSETG:	cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) >	ival(cx, ip->size, ip->regs[1]); break;
		case IR_CSETLE:	cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) <=	ival(cx, ip->size, ip->regs[1]); break;
		case IR_CSETGE:	cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) >=	ival(cx, ip->size, ip->regs[1]); break;

		case IR_CSETB:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) <	uval(cx, ip->size, ip->regs[1]); break;
		case IR_CSETA:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) >	uval(cx, ip->size, ip->regs[1]); break;
		case IR_CSETBE:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) <=	uval(cx, ip->size, ip->regs[1]); break;
		case IR_CSETAE:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) >=	uval(cx, ip->size, ip->regs[1]); break;

		case IR_CSETE:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) ==	uval(cx, ip->size, ip->regs[1]); break;
		case IR_CSETNE:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) !=	uval(cx, ip->size, ip->regs[1]); break;

		case IR_AND:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) &	uval(cx, ip->size, ip->regs[1]); break;
		case IR_OR:		cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) |	uval(cx, ip->size, ip->regs[1]); break;
		case IR_XOR:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) ^	uval(cx, ip->size, ip->regs[1]); break;
		case IR_NOT:	cx->regs[cx->reg_offs + ip->dst] = ~uval(cx, ip->size, ip->regs[0]); break;
		case IR_USHL:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) <<	uval(cx, ip->size, ip->regs[1]); break;
		case IR_ISHL:	cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) <<	uval(cx, ip->size, ip->regs[1]); break;
		case IR_USHR:	cx->regs[cx->reg_offs + ip->dst] = uval(cx, ip->size, ip->regs[0]) >>	uval(cx, ip->size, ip->regs[1]); break;
		case IR_ISHR:	cx->regs[cx->reg_offs + ip->dst] = ival(cx, ip->size, ip->regs[0]) >>	uval(cx, ip->size, ip->regs[1]); break;

		case IR_SYSCALL: {
			usz count = ip->regs[0], code = -1;
			u64 a1, a2, a3, a4;
			switch (count) {
			case 1:
				a1 = pop64(cx);
				code = syscall(a1);
				break;
			case 2:
				a2 = pop64(cx), a1 = pop64(cx);
				code = syscall(a1, a2);
				break;
			case 3:
				a3 = pop64(cx), a2 = pop64(cx), a1 = pop64(cx);
				code = syscall(a1, a2, a3);
				break;
			case 4:
				a4 = pop64(cx), a3 = pop64(cx), a2 = pop64(cx), a1 = pop64(cx);
				code = syscall(a1, a2, a3, a4);
				break;

			default:
				LT_ASSERT_NOT_REACHED();
			}
			cx->regs[cx->reg_offs + ip->dst] = code;
		}	break;

		case IR_CALL: {
			push64(cx, (u64)ip);
			cx->ip = (icode_t*)uval(cx, ISZ_64, ip->dst);
			usz reg_count = ip->regs[1];

			void* old_ret_ptr = cx->ret_ptr;
			if (ip->regs[0]) {
				if (ip->size <= ISZ_64)
					cx->ret_ptr = (u8*)&cx->regs[cx->reg_offs + ip->regs[0]];
				else {
					cx->ret_ptr = (u8*)uval(cx, ISZ_64, ip->regs[0]);
				}
			}

			cx->reg_offs += reg_count; // !!
			icode_exec(cx);
			cx->reg_offs -= reg_count; // !!

			cx->ret_ptr = old_ret_ptr;

			ip = cx->ip;
		}	break;

		case IR_JMP:
			ip = (icode_t*)uval(cx, ISZ_64, ip->dst);
			continue;

		default:
			lt_ferrf("Unhandled operation '%S'\n", icode_op_str(ip->op));
		}
		++ip;
	}
}

