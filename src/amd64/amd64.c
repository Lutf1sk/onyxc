#include <lt/io.h>
#include <lt/mem.h>
#include <lt/align.h>

#include "../interm.h"
#include "../segment.h"

#include "amd64.h"
#include "ops.h"
#include "regs.h"
#include "common.h"

static void emit_instr(amd64_ctx_t* cx, u8 op_i, u8 arg_count, amd64_ireg_t* args);

typedef
struct call_conv {
	usz reg_count;
	u8* regs;
} call_conv_t;

static
call_conv_t cconv_sysv = {
	.reg_count = 6,
	.regs = (u8[]){ REG_DI, REG_SI, REG_D, REG_C, REG_8, REG_9 },
};

static
call_conv_t cconv_linux_syscall = {
	.reg_count = 7,
	.regs = (u8[]){ REG_A, REG_DI, REG_SI, REG_D, REG_10, REG_8, REG_9 },
};

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

	#define UPD(r) (cx->reg_lifetimes[r] = i)

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
	return cx->reg_lifetimes[reg] == i;
}

static
void ireg_end(amd64_ctx_t* cx, u32 reg, usz i) {
	amd64_ireg_t* ireg = &cx->reg_map[reg];

	if ((ireg->type & ~IREG_REF) == IVAL_REG && ireg_last_use(cx, reg, i) && !reg_free(cx, ireg->mreg))
		lt_printf("Invalid free of %S (r%ud) at %uz\n", reg_names[ireg->mreg][VARG_64], reg, i);
}

static
b8 ireg_eq(amd64_ireg_t* v1, amd64_ireg_t* v2) {
	if (v1->type != v2->type || v1->disp != v2->disp)
		return 0;

	switch (v1->type & ~IREG_REF) {
	case IREG_REG: return v1->mreg == v2->mreg;
	case IREG_IMM: return v1->imm == v2->imm;
	default:
		return 0;
	}
}

static
void x64_mov(amd64_ctx_t* cx, amd64_ireg_t v1, amd64_ireg_t v2) {
	if (ireg_eq(&v1, &v2))
		return;

	amd64_ireg_t args[2] = {v1, v2};
	emit_instr(cx, X64_MOV, 2, args);
}

static
u32 init_new_reg(amd64_ctx_t* cx, u32 reg, usz i) {
	amd64_ireg_t* ireg = &cx->reg_map[reg];

	if ((ireg->type & ~IREG_REF) == IREG_REG && ireg->mreg != REG_SP && ireg_last_use(cx, reg, i)) {
		if (ireg->type & IREG_REF)
			x64_mov(cx, XREG(ireg->mreg), *ireg);
		return ireg->mreg;
	}

	u32 mreg = reg_alloc(cx, reg);
	x64_mov(cx, XREG(mreg), *ireg);
	return mreg;
}

static
void emit_instr(amd64_ctx_t* cx, u8 op_i, u8 arg_count, amd64_ireg_t* args) {
	amd64_instr_t mi;
	amd64_op_t* op = &ops[op_i];

	LT_ASSERT(arg_count <= 2);

	for (usz i = 0; i < op->var_count; ++i) {
		amd64_var_t* var = &op->vars[i];

		if ((var->args & 0b11) != arg_count)
			continue;

		memset(&mi, 0, sizeof(mi));

		for (usz i = 0; i < arg_count; ++i) {
			amd64_ireg_t* ireg = &args[i];

			u8 type = ireg->type;
			u8 varg = VARG_GET(var->args, i);

			switch (type) {
			case IREG_REG:
				LT_ASSERT(ireg->disp == 0);
				if (varg == VMOD_MRM) {
					mi.mod = MOD_REG;
					mi.reg_rm |= ireg->mreg << 4;
				}
				else if (varg == VMOD_REG)
					mi.reg_rm |= ireg->mreg;
				else
					goto next_var;
				break;

			case IREG_IMM:
			case IREG_SEG:
				if (varg != VMOD_IMM)
					goto next_var;
				mi.imm = ireg->imm;
				break;

			case IREG_REG | IREG_REF:
				if (varg != VMOD_MRM)
					goto next_var;
				mi.reg_rm |= ireg->mreg << 4;

				if (ireg->disp) {
					if (ireg->disp <= 0xFF)
						mi.mod = MOD_DSP8;
					else
						mi.mod = MOD_DSP32;
					mi.disp = ireg->disp;
				}
				else
					mi.mod = MOD_DREG;
				break;

			case IREG_IMM | IREG_REF:
			case IREG_SEG | IREG_REF:
				LT_ASSERT_NOT_REACHED();
				break;
			}
		}

		mi.op = op_i;
		mi.var = i;
		emit(cx, mi);
		return;

	next_var:
	}

	LT_ASSERT_NOT_REACHED();
}

static
void convert_icode(amd64_ctx_t* cx, seg_ent_t* seg, usz i) {
	icode_t* ir = &((icode_t*)seg->data)[i];
	amd64_instr_t mi;
	memset(&mi, 0, sizeof(mi));

	amd64_ireg_t* dst = &cx->reg_map[ir->dst];
	amd64_ireg_t* reg0 = &cx->reg_map[ir->regs[0]];
	amd64_ireg_t* reg1 = &cx->reg_map[ir->regs[1]];

	switch (ir->op) {
	case IR_INT: *dst = XIMMI(ir->uint_val); break;
	case IR_SEG: *dst = XSEG(ir->uint_val | 0x10000000); break;
	case IR_IPO: // !!
		*dst = XREG(reg_alloc(cx, ir->dst));
		ireg_end(cx, ir->dst, i);
		break;

	case IR_SRESV: {
		*dst = XREG(reg_alloc(cx, ir->dst));

		usz stack_offs = 0;
		if (cx->stack_val_it) {
			amd64_stack_val_t* v = &cx->stack_layout[cx->stack_val_it - 1];
			stack_offs = lt_align_fwd(v->offs + v->size, ir->regs[1]);
		}
		cx->stack_layout[cx->stack_val_it++] = (amd64_stack_val_t){ stack_offs, ir->regs[0], ir->regs[1] };

		write_var2(&mi, X64_LEA, VARG_64, VMOD_REG, VARG_64, VMOD_MRM);
		if (!stack_offs)
			mi.mod = MOD_DREG;
		else if (stack_offs <= 0xFF)
			mi.mod = MOD_DSP8;
		else
			mi.mod = MOD_DSP32;
		mi.disp = stack_offs;
		mi.reg_rm = dst->mreg | (REG_SP << 4);
		emit(cx, mi);

		ireg_end(cx, ir->dst, i);
	}	break;

	case IR_LOAD: {
		*dst = XREG(reg_alloc(cx, ir->dst));

		amd64_ireg_t tmp_src = *reg0;
		LT_ASSERT((!(reg0->type & IREG_REF)));
		if (reg0->type == IREG_IMM) {
			x64_mov(cx, XREG(REG_D), *reg0);
			tmp_src = XREG(REG_D);
		}
		tmp_src.type |= IREG_REF;

		x64_mov(cx, *dst, tmp_src);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
	}	break;

	case IR_STOR: {
		amd64_ireg_t tmp_dst = *dst;
		LT_ASSERT((!(dst->type & IREG_REF)));
		if (dst->type == IREG_IMM) {
			x64_mov(cx, XREG(REG_D), *dst);
			tmp_dst = XREG(REG_D);
		}
		tmp_dst.type |= IREG_REF;

		x64_mov(cx, tmp_dst, *reg0);

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
	}	break;

	case IR_TOU16:
	case IR_TOU32:
	case IR_TOU64:
		*dst = XREG(reg_alloc(cx, ir->dst));
		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	case IR_TOI16:
	case IR_TOI32:
	case IR_TOI64:
		*dst = XREG(reg_alloc(cx, ir->dst));
		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

#define GENERIC2(x) { \
		*dst = XREG(init_new_reg(cx, ir->regs[0], i)); \
		\
		amd64_ireg_t args[2] = {*dst, *reg1}; \
		emit_instr(cx, (x), 2, args); \
		\
		ireg_end(cx, ir->dst, i); \
		ireg_end(cx, ir->regs[1], i); \
}

	case IR_ADD: GENERIC2(X64_ADD); break;
	case IR_SUB: GENERIC2(X64_SUB); break;

	case IR_AND: GENERIC2(X64_AND); break;
	case IR_OR: GENERIC2(X64_OR); break;
	case IR_XOR: GENERIC2(X64_XOR); break;

#define SHIFT(x) { \
	*dst = XREG(init_new_reg(cx, ir->regs[0], i)); \
	\
	amd64_ireg_t args[2] = {*dst, *reg1}; \
	emit_instr(cx, (x), 2, args); \
	\
	ireg_end(cx, ir->dst, i); \
	ireg_end(cx, ir->regs[1], i); \
}

	case IR_USHL: SHIFT(X64_SHL); break;
	case IR_ISHL: SHIFT(X64_SAL); break;
	case IR_USHR: SHIFT(X64_SHR); break;
	case IR_ISHR: SHIFT(X64_SAR); break;

#define GENERIC3(x, res_reg) { \
	x64_mov(cx, XREG(REG_A), *reg0); \
	amd64_ireg_t arg = *reg1; \
	if (arg.type == IREG_IMM || arg.type == IREG_SEG) { \
		arg = XREG(REG_D); \
		x64_mov(cx, arg, *reg1); \
	} \
	emit_instr(cx, X64_CQO, 0, NULL); \
	emit_instr(cx, (x), 1, &arg); \
	\
	ireg_end(cx, ir->regs[0], i); \
	ireg_end(cx, ir->regs[1], i); \
	\
	*dst = XREG(reg_alloc(cx, ir->dst)); \
	x64_mov(cx, *dst, XREG(res_reg)); \
	ireg_end(cx, ir->dst, i); \
}

	case IR_IREM: GENERIC3(X64_IDIV, REG_D); break;
	case IR_UREM: GENERIC3(X64_DIV, REG_D); break;
	case IR_IDIV: GENERIC3(X64_IDIV, REG_A); break;
	case IR_UDIV: GENERIC3(X64_DIV, REG_A); break;

	case IR_IMUL: GENERIC3(X64_IMUL, REG_A); break;
	case IR_UMUL: GENERIC3(X64_MUL, REG_A); break;

#define GENERIC1(x) { \
		*dst = XREG(init_new_reg(cx, ir->regs[0], i)); \
		emit_instr(cx, (x), 1, dst); \
		ireg_end(cx, ir->dst, i); \
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
		u8 reg = cconv_linux_syscall.regs[cx->arg_num++];
		x64_mov(cx, XREG(reg), *dst);
		ireg_end(cx, ir->dst, i);
	}	break;

	case IR_SYSCALL:
		mi.op = X64_SYSCALL;
		mi.var = 0;
		emit(cx, mi);

		if (!ireg_last_use(cx, ir->dst, i)) {
			*dst = XREG(reg_alloc(cx, ir->dst));
			x64_mov(cx, *dst, XREG(REG_A));
		}

		cx->arg_num = 0;
		break;

	case IR_CSETG: case IR_CSETGE: case IR_CSETL: case IR_CSETLE:
	case IR_CSETA: case IR_CSETAE: case IR_CSETB: case IR_CSETBE:
	case IR_CSETE: case IR_CSETNE:
		*dst = XREG(reg_alloc(cx, ir->dst));
		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		ireg_end(cx, ir->regs[1], i);
		break;

	case IR_CSETZ: case IR_CSETNZ:
		*dst = XREG(reg_alloc(cx, ir->dst));
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

	case IR_JMP: {
		emit_instr(cx, X64_JMP, 1, dst);
		ireg_end(cx, ir->dst, i);
	}	break;

	case IR_CALL:
		emit_instr(cx, X64_CALL, 1, dst);

		if (!ireg_last_use(cx, ir->regs[0], i)) {
			if (ir->size <= 8) {
 				*reg0 = XREG(reg_alloc(cx, ir->regs[0]));
				x64_mov(cx, *reg0, XREG(REG_A));
 			}
		}

		ireg_end(cx, ir->dst, i);
		ireg_end(cx, ir->regs[0], i);
		break;

	case IR_RET:
		if (ir->dst) {
			if (ir->size && ir->size <= 8)
				x64_mov(cx, XREG(REG_A), *dst);
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
	cx->reg_map = lt_arena_reserve(cx->arena, sizeof(amd64_ireg_t) * 512);
	cx->reg_lifetimes = lt_arena_reserve(cx->arena, sizeof(usz) * 512);

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
				lt_printf("Leaked register %S (r%ud)\n", reg_names[i][VARG_64], cx->reg_allocated[i]);
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

