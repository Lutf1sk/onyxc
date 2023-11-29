#include <lt/debug.h>

#include "regs.h"
#include "amd64.h"
#include "ops.h"
#include "common.h"

lstr_t reg_names[AMD64_REG_COUNT][4] = {
	{ CLSTR("al"),		CLSTR("ax"),	CLSTR("eax"),	CLSTR("rax") },
	{ CLSTR("cl"),		CLSTR("cx"),	CLSTR("ecx"),	CLSTR("rcx") },
	{ CLSTR("dl"),		CLSTR("dx"),	CLSTR("edx"),	CLSTR("rdx") },
	{ CLSTR("bl"),		CLSTR("bx"),	CLSTR("ebx"),	CLSTR("rbx") },
	{ CLSTR("spl"),		CLSTR("sp"),	CLSTR("esp"),	CLSTR("rsp") },
	{ CLSTR("bpl"),		CLSTR("bp"),	CLSTR("ebp"),	CLSTR("rbp") },
	{ CLSTR("sil"),		CLSTR("si"),	CLSTR("esi"),	CLSTR("rsi") },
	{ CLSTR("dil"),		CLSTR("di"),	CLSTR("edi"),	CLSTR("rdi") },

	{ CLSTR("r8b"),		CLSTR("r8w"),	CLSTR("r8d"),	CLSTR("r8")  },
	{ CLSTR("r9b"),		CLSTR("r9w"),	CLSTR("r9d"),	CLSTR("r9")  },
	{ CLSTR("r10b"),	CLSTR("r10w"),	CLSTR("r10d"),	CLSTR("r10") },
	{ CLSTR("r11b"),	CLSTR("r11w"),	CLSTR("r11d"),	CLSTR("r11") },
	{ CLSTR("r12b"),	CLSTR("r12w"),	CLSTR("r12d"),	CLSTR("r12") },
	{ CLSTR("r13b"),	CLSTR("r13w"),	CLSTR("r13d"),	CLSTR("r13") },
	{ CLSTR("r14b"),	CLSTR("r14w"),	CLSTR("r14d"),	CLSTR("r14") },
	{ CLSTR("r15b"),	CLSTR("r15w"),	CLSTR("r15d"),	CLSTR("r15") },
};

u8 reg_flags[AMD64_REG_COUNT] = {
	0, // A
	0, // C
	0, // D
	REG_ALLOCATABLE | REG_CALLER_OWNED, // B
	REG_CALLER_OWNED, // SP
	REG_ALLOCATABLE | REG_CALLER_OWNED, // BP
	0, // SI
	0, // DI

	0, // R8
	0, // R9
	REG_SCRATCH, // R10
	REG_SCRATCH, // R11
	REG_ALLOCATABLE | REG_CALLER_OWNED, // R12
	REG_ALLOCATABLE | REG_CALLER_OWNED, // R13
	REG_ALLOCATABLE | REG_CALLER_OWNED, // R14
	REG_ALLOCATABLE | REG_CALLER_OWNED, // R15
};

u8 reg_alloc(amd64_ctx_t* cx, u32 ireg) {
	for (usz i = 0; i < AMD64_REG_COUNT; ++i) {
		if ((reg_flags[i] & REG_ALLOCATABLE) && !cx->reg_allocated[i]) {
			cx->reg_allocated[i] = ireg;
			return i;
		}
	}
// 	LT_ASSERT_NOT_REACHED();
	return REG_A;
}

u8 reg_scratch(amd64_ctx_t* cx, u32 offs) {
	for (usz i = 0; i < AMD64_REG_COUNT; ++i) {
		if (reg_flags[i] & REG_SCRATCH) {
			if (offs--)
				continue;
			return i;
		}
	}

	LT_ASSERT_NOT_REACHED();
	return REG_A;
}

u32 reg_free(amd64_ctx_t* cx, u8 reg) {
	u32 prev = cx->reg_allocated[reg];
	cx->reg_allocated[reg] = 0;
	return prev;
}

void reg_push64(amd64_ctx_t* cx, u8 reg) {
	amd64_ireg_t arg = XREG(reg, 8);
	emit_instr(cx, X64_PUSH, 1, &arg);
}

void reg_pop64(amd64_ctx_t* cx, u8 reg) {
	amd64_ireg_t arg = XREG(reg, 8);
	emit_instr(cx, X64_POP, 1, &arg);
}

void reg_push_caller_owned(amd64_ctx_t* cx) {
	for (usz i = 0; i < AMD64_REG_COUNT; ++i) {
		if ((reg_flags[i] & REG_ALLOCATABLE))
			reg_push64(cx, i);
	}
}

void reg_pop_caller_owned(amd64_ctx_t* cx) {
	for (usz i = AMD64_REG_COUNT-1; i; --i) {
		if ((reg_flags[i] & REG_ALLOCATABLE))
			reg_pop64(cx, i);
	}
}

