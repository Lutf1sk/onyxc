#include "regs.h"
#include "amd64.h"
#include "ops.h"
#include "common.h"

lstr_t reg_names[AMD64_REG_COUNT][4] = {
	{ CLSTR("al"),		CLSTR("ax"),	CLSTR("eax"),	CLSTR("rax") },
	{ CLSTR("cl"),		CLSTR("cx"),	CLSTR("ecx"),	CLSTR("rcx") },
	{ CLSTR("dl"),		CLSTR("dx"),	CLSTR("edx"),	CLSTR("rdx") },
	{ CLSTR("bl"),		CLSTR("bx"),	CLSTR("ebx"),	CLSTR("rbx") },
	{ NLSTR(),			CLSTR("sp"),	CLSTR("esp"),	CLSTR("rsp") },
	{ NLSTR(),			CLSTR("bp"),	CLSTR("ebp"),	CLSTR("rbp") },
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

u8 reg_alloc(amd64_ctx_t* cx) {
	for (usz i = 0; i < AMD64_REG_COUNT; ++i) {
		if (!cx->reg_allocated[i]) {
			cx->reg_allocated[i] = 1;
			return i;
		}
	}
	LT_ASSERT_NOT_REACHED();
	return REG_A;
}

void reg_free(amd64_ctx_t* cx, u8 reg) {
	LT_ASSERT(reg < AMD64_REG_COUNT);
	LT_ASSERT(cx->reg_allocated[reg]);
	cx->reg_allocated[reg] = 0;
}

void zero_reg(amd64_ctx_t* cx, u8 reg) {
	amd64_instr_t instr;
	instr.op = X64_XOR;
	instr.var = 0;
	instr.reg_rm = reg | (reg << 4);
	instr.mod = MOD_REG;
	emit(cx, instr);
}

