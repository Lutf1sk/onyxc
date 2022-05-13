#include <lt/io.h>
#include <lt/mem.h>
#include <lt/align.h>

#include "amd64.h"
#include "ops.h"
#include "regs.h"
#include "common.h"

#include "../interm.h"
#include "../segment.h"

static
void write_var2(amd64_instr_t* mi, u8 op_i, u8 arg1_size, u8 arg1_type, u8 arg2_size, u8 arg2_type) {
	amd64_op_t* op = &ops[op_i];

	for (usz i = 0; i < op->var_count; ++i) {
		amd64_var_t* var = &op->vars[i];
		usz arg_count = var->args & 0b11;

		if (arg_count != 2)
			continue;

		u8 size1 = VARG_GET(var->sizes, 0), size2 = VARG_GET(var->sizes, 1);
		u8 arg1 = VARG_GET(var->args, 0), arg2 = VARG_GET(var->args, 1);

		if (size1 == arg1_size && size2 == arg2_size && arg1 == arg1_type && arg2 == arg2_type) {
			mi->op = op_i;
			mi->var = i;
			return;
		}
	}

	LT_ASSERT_NOT_REACHED();
}

static
void write_var1(amd64_instr_t* mi, u8 op_i, u8 arg_size, u8 arg_type) {
	amd64_op_t* op = &ops[op_i];

	for (usz i = 0; i < op->var_count; ++i) {
		amd64_var_t* var = &op->vars[i];
		usz arg_count = var->args & 0b11;

		if (arg_count != 1)
			continue;

		u8 size = VARG_GET(var->sizes, 0);
		u8 arg = VARG_GET(var->args, 0);

		if (size == arg_size && arg == arg_type) {
			mi->op = op_i;
			mi->var = i;
			return;
		}
	}

	LT_ASSERT_NOT_REACHED();
}

static
void prepass_icode(amd64_ctx_t* cx, seg_ent_t* seg, usz i) {
	icode_t* ir = &((icode_t*)seg->data)[i];

	#define UPD(r) (cx->reg_map[(r)].last_use = i)

	switch (ir->op) {
	case IR_INT: UPD(ir->dst); break;
	case IR_FLOAT: UPD(ir->dst); break;
	case IR_SRESV: ++cx->stack_val_count; UPD(ir->dst); break;
	case IR_IPO: UPD(ir->dst); break;
	case IR_SEG: UPD(ir->dst); break;
	case IR_ENTER: break;
	case IR_CALL: UPD(ir->dst); UPD(ir->regs[0]); break;
	case IR_SYSCALL: UPD(ir->dst); break;
	default:
		if (ir->dst)
			UPD(ir->dst);

		for (usz j = 0; j < 2; ++j)
			if (ir->regs[j])
				UPD(ir->regs[j]);
		break;
	}

	#undef UPD
}

static LT_INLINE
b8 ireg_last_use(amd64_ctx_t* cx, u32 reg, usz i) {
	return cx->reg_map[reg].last_use == i;
}

static
void ireg_end(amd64_ctx_t* cx, u32 reg, usz i) {
	if (ireg_last_use(cx, reg, i)) {
		if (!reg_free(cx, cx->reg_map[reg].mreg))
			lt_printf("Invalid free of r%ud %uz\n", reg, i);
	}
}

static
void convert_icode(amd64_ctx_t* cx, seg_ent_t* seg, usz i) {
	icode_t* ir = &((icode_t*)seg->data)[i];
	amd64_instr_t mi;
	memset(&mi, 0, sizeof(mi));

	#define MREG(r) (cx->reg_map[r].mreg)
	#define ALCDST() (cx->reg_map[ir->dst].mreg = reg_alloc(cx))

	switch (ir->op) {
	case IR_INT:
		ALCDST();

		write_var2(&mi, X64_MOV, VARG_64, VMOD_MRM, VARG_32, VMOD_IMM);
		mi.imm = ir->uint_val;
		mi.mod = MOD_REG;
		mi.reg_rm = MREG(ir->dst) << 4;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		break;

// 	case IR_FLOAT:

	case IR_SRESV: {
		ALCDST();

		usz stack_offs = 0;
		if (cx->stack_val_it) {
			amd64_stack_val_t* v = &cx->stack_layout[cx->stack_val_it - 1];
			stack_offs = lt_align_fwd(v->offs + v->size, ir->regs[1]);
		}
		cx->stack_layout[cx->stack_val_it++] = (amd64_stack_val_t){ stack_offs, ir->regs[0], ir->regs[1] };

		write_var2(&mi, X64_LEA, VARG_64, VMOD_REG, VARG_64, VMOD_MRM);
		if (stack_offs <= 0xFF)
			mi.mod = MOD_DSP8;
		else
			mi.mod = MOD_DSP32;
		mi.disp = stack_offs;
		mi.reg_rm = MREG(ir->dst) | (REG_SP << 4);
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
	}	break;

	case IR_IPO:
		ALCDST();

		write_var2(&mi, X64_MOV, VARG_64, VMOD_MRM, VARG_32, VMOD_IMM);
		mi.imm = ir->uint_val | 0x10000000;
		mi.mod = MOD_REG;
		mi.reg_rm = MREG(ir->dst) << 4;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		break;

	case IR_SEG:
		ALCDST();

		write_var2(&mi, X64_MOV, VARG_64, VMOD_MRM, VARG_32, VMOD_IMM);
		mi.imm = ir->uint_val | 0x20000000;
		mi.mod = MOD_REG;
		mi.reg_rm = MREG(ir->dst) << 4;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		break;

	case IR_LOAD:
		ALCDST();

		write_var2(&mi, X64_MOV, VARG_64, VMOD_REG, VARG_64, VMOD_MRM);
		mi.mod = MOD_DREG;
		mi.reg_rm = MREG(ir->dst) | (MREG(ir->regs[0]) << 4);
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	case IR_STOR:
		write_var2(&mi, X64_MOV, VARG_64, VMOD_MRM, VARG_64, VMOD_REG);
		mi.mod = MOD_DREG;
		mi.reg_rm = (MREG(ir->dst) << 4) | MREG(ir->regs[0]);
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	case IR_TOU16:
	case IR_TOU32:
	case IR_TOU64:
		ALCDST();

		mi.op = X64_MOVZX;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	case IR_TOI16:
	case IR_TOI32:
	case IR_TOI64:
		ALCDST();

		mi.op = X64_MOVSX;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

#define GENERIC2(x) { \
		if (ireg_last_use(cx, ir->regs[0], i)) \
			MREG(ir->dst) = MREG(ir->regs[0]); \
		else { \
			ALCDST(); \
			\
			write_var2(&mi, X64_MOV, VARG_64, VMOD_REG, VARG_64, VMOD_MRM); \
			mi.mod = MOD_REG; \
			mi.reg_rm = MREG(ir->dst) | (MREG(ir->regs[0]) << 4); \
			emit(cx, mi); \
			\
			ireg_end(cx, ir->regs[0], i); \
		} \
		\
		write_var2(&mi, (x), VARG_64, VMOD_REG, VARG_64, VMOD_MRM); \
		mi.mod = MOD_REG; \
		mi.reg_rm = MREG(ir->dst) | (MREG(ir->regs[1]) << 4); \
		emit(cx, mi); \
		\
		ireg_end(cx, ir->dst, i); \
		ireg_end(cx, ir->regs[1], i); \
}

	case IR_ADD: GENERIC2(X64_ADD); break;
	case IR_SUB: GENERIC2(X64_SUB); break;

	case IR_AND: GENERIC2(X64_AND); break;
	case IR_OR: GENERIC2(X64_OR); break;
	case IR_XOR: GENERIC2(X64_XOR); break;

	case IR_USHL:
		ALCDST();

		mi.op = X64_SHL;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_ISHL:
		ALCDST();

		mi.op = X64_SAL;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_USHR:
		ALCDST();

		mi.op = X64_SHR;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_ISHR:
		ALCDST();

		mi.op = X64_SAR;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_IREM:
		ALCDST();

		mi.op = X64_IDIV;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_UREM:
		ALCDST();

		mi.op = X64_DIV;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_IDIV:
		ALCDST();

		mi.op = X64_IDIV;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_UDIV:
		ALCDST();

		mi.op = X64_DIV;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_IMUL:
		ALCDST();

		mi.op = X64_IMUL;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_UMUL:
		ALCDST();

		mi.op = X64_MUL;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

#define GENERIC1(x) {\
		if (ireg_last_use(cx, ir->regs[0], i)) \
			MREG(ir->dst) = MREG(ir->regs[0]); \
		else { \
			ALCDST(); \
			\
			write_var2(&mi, X64_MOV, VARG_64, VMOD_REG, VARG_64, VMOD_MRM); \
			mi.mod = MOD_REG; \
			mi.reg_rm = MREG(ir->dst) | (MREG(ir->regs[0]) << 4); \
			emit(cx, mi); \
			\
			ireg_end(cx, ir->regs[0], i); \
		} \
		\
		write_var1(&mi, (x), VARG_64, VMOD_MRM); \
		mi.mod = MOD_REG; \
		mi.reg_rm = MREG(ir->regs[0]) << 4; \
		emit(cx, mi); \
		\
		ireg_end(cx, ir->dst, i); \
		ireg_end(cx, ir->regs[0], i); \
}

	case IR_NEG: GENERIC1(X64_NEG); break;
	case IR_INC: GENERIC1(X64_INC); break;
	case IR_DEC: GENERIC1(X64_DEC); break;

	case IR_ENTER:
		mi.op = X64_IR_ENTER;
		emit(cx, mi);
		break;

	case IR_GETARG:
		ireg_end(cx, ir->dst, i);
		break;

	case IR_SETARG: { // !!
		u32 reg = 0;
		switch (cx->arg_num++) {
		case 0: reg = REG_A; break;
		case 1: reg = REG_DI; break;
		case 2: reg = REG_SI; break;
		case 3: reg = REG_D; break;
		case 4: reg = REG_10; break;
		case 5: reg = REG_8; break;
		case 6: reg = REG_9; break;
		}

		write_var2(&mi, X64_MOV, VARG_64, VMOD_MRM, VARG_64, VMOD_REG);
		mi.mod = MOD_REG;
		mi.reg_rm = (reg << 4) | MREG(ir->dst);
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
	}	break;

	case IR_SYSCALL:
		ALCDST();

		for (usz i = cx->arg_num; i < 7; ++i) {
			switch (cx->arg_num++) {
			case 0: zero_reg(cx, REG_A); break;
			case 1: zero_reg(cx, REG_DI); break;
			case 2: zero_reg(cx, REG_SI); break;
			case 3: zero_reg(cx, REG_D); break;
			case 4: zero_reg(cx, REG_10); break;
			case 5: zero_reg(cx, REG_8); break;
			case 6: zero_reg(cx, REG_9); break;
			}
		}

		mi.op = X64_SYSCALL;
		mi.var = 0;
		emit(cx, mi);

		write_var2(&mi, X64_MOV, VARG_64, VMOD_REG, VARG_64, VMOD_MRM);
		mi.mod = MOD_REG;
		mi.reg_rm = MREG(ir->dst) | (REG_A << 4);
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);

		cx->arg_num = 0;
		break;

	case IR_CSETG: case IR_CSETGE: case IR_CSETL: case IR_CSETLE:
	case IR_CSETA: case IR_CSETAE: case IR_CSETB: case IR_CSETBE:
	case IR_CSETE: case IR_CSETNE:
		ALCDST();

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_CSETZ: case IR_CSETNZ:
		ALCDST();

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	case IR_CJMPG: case IR_CJMPGE: case IR_CJMPL: case IR_CJMPLE:
	case IR_CJMPA: case IR_CJMPAE: case IR_CJMPB: case IR_CJMPBE:
	case IR_CJMPE: case IR_CJMPNE:
		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_CJMPZ: case IR_CJMPNZ:
		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	case IR_JMP:
		write_var1(&mi, X64_JMP, VARG_64, VMOD_MRM);
		mi.mod = MOD_REG;
		mi.reg_rm = MREG(ir->dst) << 4;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		break;

	case IR_CALL:
		if (ir->size <= 8)
			cx->reg_map[ir->regs[0]].mreg = reg_alloc(cx);

		write_var1(&mi, X64_CALL, VARG_64, VMOD_MRM);
		mi.mod = MOD_REG;
		mi.reg_rm = MREG(ir->dst) << 4;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	case IR_RET:
		if (ir->dst) {
			if (ir->size && ir->size <= 8) {
				write_var2(&mi, X64_MOV, VARG_64, VMOD_REG, VARG_64, VMOD_MRM);
				mi.mod = MOD_REG;
				mi.reg_rm = REG_A | (MREG(ir->dst) << 4);
				emit(cx, mi);
			}
			ireg_end(cx, ir->dst, i);
		}
		mi.op = X64_IR_RET;
		emit(cx, mi);
		break;

	case IR_MCOPY:
		mi.op = X64_MOVSB;
		mi.prefix[0] = PFX_REP;
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	default:
		lt_printf("Unhandled intermediate instruction %S\n", icode_op_str(ir->op));
		LT_ASSERT_NOT_REACHED();
	}
}

void amd64_gen(amd64_ctx_t* cx) {
	usz rmap_size = sizeof(amd64_ireg_t) * 512;
	cx->reg_map = lt_arena_reserve(cx->arena, rmap_size);

	usz seg_count = cx->seg_count;

	for (usz i = 0; i < seg_count; ++i) {
		seg_ent_t* cs = &cx->seg[i];

		if (cs->stype != SEG_ICODE)
			continue;

		memset(cx->reg_allocated, 0, sizeof(cx->reg_allocated));

		cx->curr_func = new_mcode_seg(cx, cs->type, cs->name);
		cx->stack_val_count = 0;
		cx->stack_val_it = 0;
		cx->arg_num = 0;

		lt_printf("Assembling CS %uq -> M-CS %uq\n", i, cx->curr_func);

		for (usz i = 0; i < cs->size; ++i)
			prepass_icode(cx, cs, i);

		cx->stack_layout = lt_arena_reserve(cx->arena, cx->stack_val_count * sizeof(amd64_stack_val_t));

		for (usz i = 0; i < cs->size; ++i)
			convert_icode(cx, cs, i);

		for (usz i = 0 ; i < AMD64_REG_COUNT; ++i)
			if (cx->reg_allocated[i])
				lt_printf("Leaked register %S\n", reg_names[i][VARG_64]);
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

void amd64_print_seg(amd64_ctx_t* cx, usz i) {
	amd64_instr_t* mcode = cx->seg[i].data;
	usz mcode_count = cx->seg[i].size;

	for (usz i = 0; i < mcode_count; ++i) {
		lt_printc('\t');
		amd64_print_instr(&mcode[i]);
		lt_printc('\n');
	}
}

void amd64_print_instr(amd64_instr_t* instr) {
	amd64_op_t* op = &ops[instr->op];
	amd64_var_t* var = &op->vars[instr->var];

	if (!op->var_count) {
		lt_printf("0 %S", op->str);
		return;
	}

	usz arg_count = var->args & 0b11;

	lt_printf("%ud %S ", arg_count, op->str);

	u8 reg = instr->reg_rm & 0b1111;
	u8 rm = instr->reg_rm >> 4;

	for (usz i = 0; i < arg_count; ++i) {
		if (i)
			lt_printls(CLSTR(", "));

		u8 size = VARG_GET(var->sizes, i);

		switch (VARG_GET(var->args, i)) {
		case VMOD_REG: lt_printf("%S", reg_names[reg][size]); break;
		case VMOD_MRM: print_modrm(instr->mod, rm, size, instr->disp); break;
		case VMOD_IMM: lt_printf("0x%hq", instr->imm); break;
		}
	}
}

