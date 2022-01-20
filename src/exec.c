#include "exec.h"

#include "segment.h"
#include "interm.h"

#include <lt/mem.h>
#include <lt/io.h>
#include <lt/align.h>

u64 syscall(u64, ...);

#define ivext(iv) (sign_extend((iv).size, val(cx, (iv))))

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
void* ref(exec_ctx_t* cx, ival_t v) {
	switch (v.stype) {
	case IVAL_REG:
		LT_ASSERT(!v.disp);
		return &cx->regs[cx->reg_offs + v.reg];

	case IVAL_REG | IVAL_REF: return (u8*)cx->regs[cx->reg_offs + v.reg] + v.disp; break;
	case IVAL_IMM | IVAL_REF: return (u8*)v.uint_val + v.disp; break;
	case IVAL_DSO | IVAL_REF: return (u8*)cx->ds[v.dso].data + v.disp; break;
	case IVAL_CSO | IVAL_REF: return (u8*)cx->cs[v.cso].data + v.disp; break;

	default:
		LT_ASSERT_NOT_REACHED();
		return NULL;
	}
}

static
u64 val(exec_ctx_t* cx, ival_t v) {
	u8* ptr = NULL;

	switch (v.stype) {
	case IVAL_IMM:
		LT_ASSERT(!v.disp);
		return v.uint_val;

	case IVAL_REG: ptr = ref(cx, v); break;
	case IVAL_DSO: return (usz)cx->ds[v.dso].data + v.disp;
	case IVAL_CSO: return (usz)cx->cs[v.cso].data + v.disp;

	default:
		if (v.stype & IVAL_REF) {
			ptr = ref(cx, v);
			break;
		}
		LT_ASSERT_NOT_REACHED();
	}

	switch (v.size) {
	case 1: return *(u8*)ptr;
	case 2: return *(u16*)ptr;
	case 4: return *(u32*)ptr;
	case 8: return *(u64*)ptr;

	default:
		LT_ASSERT_NOT_REACHED();
	}

	return 0;
}

static LT_INLINE
void mov(exec_ctx_t* cx, ival_t dst, u64 v) {
	void* dst_ptr = ref(cx, dst);

	switch (dst.size) {
	case 1: *(u8*)dst_ptr = v; break;
	case 2: *(u16*)dst_ptr = v; break;
	case 4: *(u32*)dst_ptr = v; break;
	case 8: *(u64*)dst_ptr = v; break;

	default:
		LT_ASSERT_NOT_REACHED();
	}
}

static LT_INLINE
u64 pop64(exec_ctx_t* cx) {
	return *(u64*)(cx->sp -= 8);
}

static
void pop(exec_ctx_t* cx, ival_t dst) {
	void* dst_ptr = ref(cx, dst);
	memcpy(dst_ptr, cx->sp -= dst.size, dst.size);
}


static LT_INLINE
void push64(exec_ctx_t* cx, u64 val) {
	*(u64*)cx->sp = val;
	cx->sp += 8;
}

static
void push(exec_ctx_t* cx, ival_t ival) {
	u64 v = 0;
	void* ptr = &v;
	if (ival.stype & IVAL_REF || ival.stype == IVAL_REG)
		ptr = ref(cx, ival);
	else
		v = val(cx, ival);
	memcpy(cx->sp, ptr, ival.size);
	cx->sp += ival.size;
}

void icode_exec(exec_ctx_t* cx) {
	icode_t* ip = cx->ip;
	for (;;) {
		switch (ip->op) {
		case IR_NEG: mov(cx, ip->arg1, -val(cx, ip->arg2)); break;

		case IR_INC: mov(cx, ip->arg1, val(cx, ip->arg1) + 1); break;
		case IR_DEC: mov(cx, ip->arg1, val(cx, ip->arg1) - 1); break;

		case IR_ADD: mov(cx, ip->arg1, val(cx, ip->arg2) + val(cx, ip->arg3)); break;
		case IR_SUB: mov(cx, ip->arg1, val(cx, ip->arg2) - val(cx, ip->arg3)); break;

		case IR_IMUL: mov(cx, ip->arg1, ivext(ip->arg2) * ivext(ip->arg3)); break;
		case IR_IDIV: mov(cx, ip->arg1, ivext(ip->arg2) / ivext(ip->arg3)); break;
		case IR_IREM: mov(cx, ip->arg1, ivext(ip->arg2) % ivext(ip->arg3)); break;

		case IR_UMUL: mov(cx, ip->arg1, val(cx, ip->arg2) * val(cx, ip->arg3)); break;
		case IR_UDIV: mov(cx, ip->arg1, val(cx, ip->arg2) / val(cx, ip->arg3)); break;
		case IR_UREM: mov(cx, ip->arg1, val(cx, ip->arg2) % val(cx, ip->arg3)); break;

		case IR_SRESV: {
			u64 size = val(cx, ip->arg2);
			u64 align = val(cx, ip->arg3);
			usz padding = lt_pad((usz)cx->sp, align);
			mov(cx, ip->arg1, (u64)cx->sp + padding);
			cx->sp += padding + size;
		}	break;

		case IR_MOV: mov(cx, ip->arg1, val(cx, ip->arg2)); break;
		case IR_LEA: mov(cx, ip->arg1, (u64)ref(cx, ip->arg2)); break;
		case IR_COPY: memcpy(ref(cx, ip->arg1), ref(cx, ip->arg2), ip->arg1.size); break;

		case IR_IEXT: mov(cx, ip->arg1, ivext(ip->arg2)); break;
		case IR_UEXT: mov(cx, ip->arg1, val(cx, ip->arg2)); break;

		case IR_ENTER:
			push64(cx, (u64)cx->bp);
			cx->bp = cx->sp;
			break;

		case IR_GETARG: {
			void* dst = ref(cx, ip->arg1);
			void* src = cx->bp - (val(cx, ip->arg2) + 16 + ip->arg1.size);
			memcpy(dst, src, ip->arg1.size);
		}	break;

		case IR_SETARG:
			push(cx, ip->arg2);
			break;

		case IR_RET: {
			if (ip->arg1.stype != IVAL_INVAL) {
				if (ip->arg1.stype & IVAL_REF || ip->arg1.stype == IVAL_REG)
					memcpy(cx->ret_ptr, ref(cx, ip->arg1), ip->arg1.size);
				else {
					u64 v = val(cx, ip->arg1);
					LT_ASSERT(cx->ret_ptr);
					memcpy(cx->ret_ptr, &v, ip->arg1.size);
				}
			}
			cx->sp = cx->bp;
			cx->bp = (u8*)pop64(cx);
			cx->ip = (icode_t*)pop64(cx);
			return;
		}

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

		case IR_CJMPA:
			if (val(cx, ip->arg2) > val(cx, ip->arg3)) {
				ip = (icode_t*)val(cx, ip->arg1);
				continue;
			}
			break;

		case IR_CJMPAE:
			if (val(cx, ip->arg2) >= val(cx, ip->arg3)) {
				ip = (icode_t*)val(cx, ip->arg1);
				continue;
			}
			break;

		case IR_CSETZ: mov(cx, ip->arg1, !val(cx, ip->arg2)); break;
		case IR_CSETNZ: mov(cx, ip->arg1, !!val(cx, ip->arg2)); break;

		case IR_CSETL: mov(cx, ip->arg1, ivext(ip->arg2) < ivext(ip->arg3)); break;
		case IR_CSETG: mov(cx, ip->arg1, ivext(ip->arg2) > ivext(ip->arg3)); break;
		case IR_CSETLE: mov(cx, ip->arg1, ivext(ip->arg2) <= ivext(ip->arg3)); break;
		case IR_CSETGE: mov(cx, ip->arg1, ivext(ip->arg2) >= ivext(ip->arg3)); break;

		case IR_CSETB: mov(cx, ip->arg1, val(cx, ip->arg2) < val(cx, ip->arg3)); break;
		case IR_CSETA: mov(cx, ip->arg1, val(cx, ip->arg2) > val(cx, ip->arg3)); break;
		case IR_CSETBE: mov(cx, ip->arg1, val(cx, ip->arg2) <= val(cx, ip->arg3)); break;
		case IR_CSETAE: mov(cx, ip->arg1, val(cx, ip->arg2) >= val(cx, ip->arg3)); break;

		case IR_CSETE: mov(cx, ip->arg1, val(cx, ip->arg2) == val(cx, ip->arg3)); break;
		case IR_CSETNE: mov(cx, ip->arg1, val(cx, ip->arg2) != val(cx, ip->arg3)); break;

		case IR_AND: mov(cx, ip->arg1, val(cx, ip->arg2) & val(cx, ip->arg3)); break;
		case IR_OR: mov(cx, ip->arg1, val(cx, ip->arg2) | val(cx, ip->arg3)); break;
		case IR_XOR: mov(cx, ip->arg1, val(cx, ip->arg2) ^ val(cx, ip->arg3)); break;
		case IR_NOT: mov(cx, ip->arg1, ~val(cx, ip->arg2)); break;
		case IR_USHL: mov(cx, ip->arg1, val(cx, ip->arg2) << val(cx, ip->arg3)); break;
		case IR_ISHL: mov(cx, ip->arg1, (i64)val(cx, ip->arg2) << (i64)val(cx, ip->arg3)); break;
		case IR_USHR: mov(cx, ip->arg1, val(cx, ip->arg2) >> val(cx, ip->arg3)); break;
		case IR_ISHR: mov(cx, ip->arg1, (i64)val(cx, ip->arg2) >> (i64)val(cx, ip->arg3)); break;

		case IR_SYSCALL: {
			usz count = val(cx, ip->arg2), code = -1;
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
			mov(cx, ip->arg1, code);
		}	break;

		case IR_CALL: {
			push64(cx, (u64)ip);
			cx->ip = (icode_t*)val(cx, ip->arg2);
			usz stack_pop = val(cx, ip->arg3);

			void* old_ret_ptr = cx->ret_ptr;
			if (ip->arg1.stype != IVAL_INVAL)
				cx->ret_ptr = ref(cx, ip->arg1);

			cx->reg_offs += 32; // !!
			icode_exec(cx);
			cx->reg_offs -= 32; // !!

			cx->ret_ptr = old_ret_ptr;

			ip = cx->ip;
			cx->sp -= stack_pop;
		}	break;

		case IR_JMP:
			ip = (icode_t*)val(cx, ip->arg1);
			continue;

		default:
			lt_ferrf("Unhandled operation '%S'\n", icode_type_str(ip->op));
		}
		++ip;
	}
}

