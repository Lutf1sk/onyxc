#include <lt/io.h>
#include <lt/mem.h>

#include "amd64.h"
#include "ops.h"
#include "regs.h"
#include "common.h"

#include "../interm.h"
#include "../segment.h"

static
void convert_icode(amd64_ctx_t* cx, seg_ent_t* seg, usz i) {
	icode_t* ir = &((icode_t*)seg->data)[i];

	switch (ir->op) {
	case IR_INT: break;
	case IR_FLOAT: break;

	case IR_IPO: break;
	case IR_SEG: break;

	case IR_LOAD: break;
	case IR_STOR: break;

	case IR_TOU16:
	case IR_TOU32:
	case IR_TOU64: break;

	case IR_TOI16:
	case IR_TOI32:
	case IR_TOI64: break;

	case IR_ADD: break;

	case IR_SUB: break;

	case IR_AND: break;
	case IR_OR: break;
	case IR_XOR: break;

	case IR_USHL:break;
	case IR_USHR: case IR_ISHR: break;
	case IR_ISHL: break;

	case IR_IREM:
		break;

	case IR_UREM:
		break;

	case IR_IDIV:
		break;

	case IR_UDIV:
		break;

	case IR_IMUL:
		break;

	case IR_UMUL:
		break;

	case IR_NEG: break;

	case IR_INC: break;
	case IR_DEC: break;

	case IR_ENTER:
// 		i.op = X64_IR_ENTER;
// 		emit(cx, i);
		break;

	case IR_GETARG:
		break;

	case IR_SETARG:
		break;

	case IR_SYSCALL:
// 		i.op = X64_SYSCALL;
// 		emit(cx, i);
		break;

	case IR_SRESV:
		break;

	case IR_CSETG: case IR_CSETGE: case IR_CSETL: case IR_CSETLE:
	case IR_CSETA: case IR_CSETAE: case IR_CSETB: case IR_CSETBE:
	case IR_CSETE: case IR_CSETNE: case IR_CSETZ: case IR_CSETNZ:
		break;

	case IR_CJMPG: case IR_CJMPGE: case IR_CJMPL: case IR_CJMPLE:
	case IR_CJMPA: case IR_CJMPAE: case IR_CJMPB: case IR_CJMPBE:
	case IR_CJMPE: case IR_CJMPNE: case IR_CJMPZ: case IR_CJMPNZ:
		break;

	case IR_JMP: break;
	case IR_CALL: break;

	case IR_RET:
// 		i.op = X64_IR_LEAVE;
// 		emit(cx, i);
// 		i.op = X64_RET;
// 		emit(cx, i);
		break;

	default:
		LT_ASSERT_NOT_REACHED();
	}
}

void amd64_gen(amd64_ctx_t* cx) {
	usz rmap_size = sizeof(amd64_mval_t) * 256;
	cx->reg_map = lt_arena_reserve(cx->arena, rmap_size);

	usz seg_count = cx->seg_count;

	for (usz i = 0; i < seg_count; ++i) {
		seg_ent_t* cs = &cx->seg[i];

		if (cs->stype != SEG_ICODE)
			continue;

		memset(cx->reg_allocated, 0, sizeof(cx->reg_allocated));
		cx->reg_allocated[REG_A] = 1;
		cx->reg_allocated[REG_C] = 1;
		cx->reg_allocated[REG_D] = 1;
		cx->reg_allocated[REG_SP] = 1;
		cx->reg_allocated[REG_BP] = 1;
		cx->reg_allocated[REG_12] = 1;
		cx->reg_allocated[REG_13] = 1;

		memset(cx->reg_map, 0, rmap_size);

		lt_printf("Assembling cs'%uq\n", i);

		cx->curr_func = new_mcode_seg(cx, cs->type);

		for (usz j = 0; j < cs->size; ++j)
			convert_icode(cx, cs, j);
	}
}

static
void print_modrm(u8 mod, u8 rm, u8 size, u32 disp) {
	switch (mod) {
		// TODO: Handle SIB and RIP-relative addressing modes
		case MOD_DREG: lt_printf("[%S]", reg_names[rm][size]); break;
		case MOD_DSP8: lt_printf("[%S + %id]", reg_names[rm][size], disp); break;
		case MOD_DSP32: lt_printf("[%S + %id]", reg_names[rm][size], disp); break;
		case MOD_REG: lt_printf("%S", reg_names[rm][size]); break;
	}
}

void amd64_print_instr(amd64_instr_t instr) {
	amd64_op_t* op = &ops[instr.op];
	amd64_var_t* var = &op->vars[instr.var];

	usz arg_count = var->args & 0b11;

	lt_printf("%ud %S ", arg_count, op->str);

	u8 reg = instr.reg_rm & 0b1111;
	u8 rm = instr.reg_rm >> 4;
	u8 sizes = var->sizes, args = var->args;

	while (arg_count--) {
		u8 size = (sizes >>= 2) & 0b11;

		switch ((args >>= 2) & 0b11) {
		case VMOD_REG: lt_printf("%S", reg_names[reg][size]); break;
		case VMOD_MRM: print_modrm(instr.mod, rm, size, instr.disp); break;
		case VMOD_IMM: lt_printf("%iq", instr.imm); break;
		}

		if (arg_count)
			lt_printls(CLSTR(", "));
	}
}

