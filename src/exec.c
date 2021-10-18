#include "exec.h"

#include "segment.h"
#include "interm.h"

u64 syscall(u64, ...);

static
i64 sign_extend(usz to, u64 v) {
	switch (to) {
	case 1: return (i8)v;
	case 2: return (i16)v;
	case 4: return (i32)v;
	case 8: return (i64)v;
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

static LT_INLINE
u64 val(exec_ctx_t* cx, ival_t v) {
	u8* ptr;

	switch (v.stype) {
	case IVAL_REG: return cx->regs[v.reg];
	case IVAL_IMM: return v.uint_val;
	case IVAL_DSO: return (usz)cx->ds[v.dso].data + v.index * v.scale;
	case IVAL_CSO: return (usz)cx->cs[v.cso].data + v.index * v.scale;
	case IVAL_SFO: return (usz)cx->bp + v.sfo + v.index * v.scale;

	case IVAL_REG | IVAL_REF: ptr = (u8*)cx->regs[v.reg]; break;
	case IVAL_IMM | IVAL_REF: ptr = (u8*)v.uint_val; break;
	case IVAL_DSO | IVAL_REF: ptr = (u8*)cx->ds[v.dso].data; break;
	case IVAL_CSO | IVAL_REF: ptr = (u8*)((usz)cx->cs[v.cso].data); break;
	case IVAL_SFO | IVAL_REF: ptr = (u8*)((usz)cx->bp + v.sfo); break;
	}

	switch (v.size) {
	case 1: return *(u8*)(ptr + v.index * v.scale);
	case 2: return *(u16*)(ptr + v.index * v.scale);
	case 4: return *(u32*)(ptr + v.index * v.scale);
	case 8: return *(u64*)(ptr + v.index * v.scale);
	}

	LT_ASSERT_NOT_REACHED();
	return 0;
}

static LT_INLINE
void mov(exec_ctx_t* cx, ival_t dst, u64 v) {
	if (dst.stype & IVAL_REF) {
		dst.stype &= ~IVAL_REF;
		dst.size = 8;
		switch (dst.size) {
		case 1: *(u8*)val(cx, dst) = v; break;
		case 2: *(u16*)val(cx, dst) = v; break;
		case 4: *(u32*)val(cx, dst) = v; break;
		case 8: *(u64*)val(cx, dst) = v; break;
		}
		return;
	}
	cx->regs[dst.reg] = v;
}

static LT_INLINE
u64 pop64(exec_ctx_t* cx) {
	return *(u64*)(cx->sp -= 8);
}

static LT_INLINE
void push8(exec_ctx_t* cx, u8 val) {
	*(u8*)cx->sp = val;
	cx->sp += 1;
}

static LT_INLINE
void push16(exec_ctx_t* cx, u16 val) {
	*(u16*)cx->sp = val;
	cx->sp += 2;
}

static LT_INLINE
void push32(exec_ctx_t* cx, u32 val) {
	*(u32*)cx->sp = val;
	cx->sp += 4;
}

static LT_INLINE
void push64(exec_ctx_t* cx, u64 val) {
	*(u64*)cx->sp = val;
	cx->sp += 8;
}

static
void push(exec_ctx_t* cx, ival_t ival) {
	switch (ival.size) {
	case 1: push8(cx, val(cx, ival)); break;
	case 2: push16(cx, val(cx, ival)); break;
	case 4: push32(cx, val(cx, ival)); break;
	case 8: push64(cx, val(cx, ival)); break;
	}
}

u64 icode_exec(exec_ctx_t* cx) {
	icode_t* ip = cx->ip;
	for (;;) {
		switch (ip->op) {
		case IR_UEXT:
			mov(cx, ip->arg1, val(cx, ip->arg2));
			break;

		case IR_IEXT:
			mov(cx, ip->arg1, sign_extend(ip->arg2.size, val(cx, ip->arg2)));
			break;

		case IR_NEG:
			mov(cx, ip->arg1, -val(cx, ip->arg2));
			break;

		case IR_INC:
			mov(cx, ip->arg1, val(cx, ip->arg1) + 1);
			break;

		case IR_ADD:
			mov(cx, ip->arg1, val(cx, ip->arg2) + val(cx, ip->arg3));
			break;

		case IR_SUB:
			mov(cx, ip->arg1, val(cx, ip->arg2) + val(cx, ip->arg3));
			break;

		case IR_IMUL:
			mov(cx, ip->arg1, (i64)val(cx, ip->arg2) * (i64)val(cx, ip->arg3));
			break;

		case IR_IDIV:
			mov(cx, ip->arg1, (i64)val(cx, ip->arg2) / (i64)val(cx, ip->arg3));
			break;

		case IR_IREM:
			mov(cx, ip->arg1, (i64)val(cx, ip->arg2) % (i64)val(cx, ip->arg3));
			break;

		case IR_UMUL:
			mov(cx, ip->arg1, val(cx, ip->arg2) * val(cx, ip->arg3));
			break;

		case IR_UDIV:
			mov(cx, ip->arg1, val(cx, ip->arg2) / val(cx, ip->arg3));
			break;

		case IR_UREM:
			mov(cx, ip->arg1, val(cx, ip->arg2) % val(cx, ip->arg3));
			break;

		case IR_MOV:
			mov(cx, ip->arg1, val(cx, ip->arg2));
			break;

		case IR_SRESV:
			cx->sp += val(cx, ip->arg1);
			break;

		case IR_PUSH:
			push(cx, ip->arg1);
			break;

		case IR_ENTER:
			push64(cx, (u64)cx->bp);
			cx->bp = cx->sp;
			break;

		case IR_GETARG:
			mov(cx, ip->arg1, cx->args[ip->arg2.uint_val]);
			break;

		case IR_SETARG:
			cx->args[ip->arg1.uint_val] = val(cx, ip->arg2);
			break;

		case IR_RET:
			if (ip->arg1.stype != IVAL_INVAL)
				cx->retval = val(cx, ip->arg1);
			cx->sp = cx->bp;
			cx->bp = (u8*)pop64(cx);
			ip = (icode_t*)pop64(cx);
			continue;

		case IR_RETVAL:
			mov(cx, ip->arg1, cx->retval);
			break;

		case IR_CJMPZ:
			if (!val(cx, ip->arg2)) {
				ip = (icode_t*)val(cx, ip->arg1);
				continue;
			}
			break;

		case IR_CJMPNZ:
			if (val(cx, ip->arg2)) {
				ip = (icode_t*)val(cx, ip->arg1);
				continue;
			}
			break;

		case IR_CSETZ:	if (!val(cx, ip->arg2)) mov(cx, ip->arg1, 1); break;
		case IR_CSETNZ:	if (val(cx, ip->arg2)) mov(cx, ip->arg1, 1); break;
		case IR_CSETL:	if (val(cx, ip->arg2) < val(cx, ip->arg3)) mov(cx, ip->arg1, 1); break;
		case IR_CSETG:	if (val(cx, ip->arg2) > val(cx, ip->arg3)) mov(cx, ip->arg1, 1); break;
		case IR_CSETLE:	if (val(cx, ip->arg2) <= val(cx, ip->arg3)) mov(cx, ip->arg1, 1); break;
		case IR_CSETGE:	if (val(cx, ip->arg2) >= val(cx, ip->arg3)) mov(cx, ip->arg1, 1); break;
		case IR_CSETE:	if (val(cx, ip->arg2) == val(cx, ip->arg3)) mov(cx, ip->arg1, 1); break;
		case IR_CSETNE:	if (val(cx, ip->arg2) != val(cx, ip->arg3)) mov(cx, ip->arg1, 1); break;

		case IR_SYSCALL: {
			usz count = val(cx, ip->arg2), code = -1;
			switch (count) {
			case 1: code = syscall(cx->args[0]); break;
			case 2: code = syscall(cx->args[0], cx->args[1]); break;
			case 3: code = syscall(cx->args[0], cx->args[1], cx->args[2]); break;
			case 4: code = syscall(cx->args[0], cx->args[1], cx->args[2], cx->args[3]); break;
			}
			mov(cx, ip->arg1, code);
		}	break;

		case IR_CALL:
			push64(cx, (u64)(ip + 1));
		case IR_JMP:
			ip = (icode_t*)val(cx, ip->arg1);
			continue;

		case IR_EXIT:
			return val(cx, ip->arg1);

		default:
			lt_ferrf("Unhandled operation '%S'\n", icode_type_str(ip->op));
		}
		++ip;
	}
}

