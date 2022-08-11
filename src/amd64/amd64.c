#include <lt/io.h>
#include <lt/mem.h>
#include <lt/align.h>

#include "../interm.h"
#include "../segment.h"
#include "../textattrib.h"

#include "amd64.h"
#include "ops.h"
#include "regs.h"
#include "common.h"
#include "lbl.h"

typedef
struct call_conv {
	usz reg_count;
	u8* regs;
} call_conv_t;

static
call_conv_t cconvs[] = {
#define CCONV_SYSV 0
	{
		.reg_count = 6,
		.regs = (u8[]){ REG_DI, REG_SI, REG_D, REG_C, REG_8, REG_9 },
	},
#define CCONV_LINUX_SYSCALL 1
	{
		.reg_count = 7,
		.regs = (u8[]){ REG_A, REG_DI, REG_SI, REG_D, REG_10, REG_8, REG_9 },
	},
};

#define IR_REG_DST	1
#define IR_REG_R0	2
#define IR_REG_R1	4

static
u8 ir_get_regs(icode_t* ir) {
	switch (ir->op) {
	case IR_SYSCALL: case IR_SETARG: case IR_GETARG:
	case IR_INT: case IR_FLOAT: case IR_GETLBL: case IR_SRESV: case IR_SEG:
		return IR_REG_DST;
	case IR_ENTER: case IR_BEGCALL: case IR_LBL: return 0;

	case IR_CALL: return IR_REG_DST|IR_REG_R0;

	default: {
		u8 regs = 0;
		if (ir->dst)
			regs |= IR_REG_DST;
		if (ir->regs[0])
			regs |= IR_REG_R0;
		if (ir->regs[1])
			regs |= IR_REG_R1;
		return regs;
	}
	}
}

static
u32 sresv(amd64_ctx_t* cx, usz size, usz align) {
	u32 offs = lt_align_fwd(cx->stack_top, align);
	cx->stack_top = offs + size;
	return offs;
}

static
void prepass_icode(amd64_ctx_t* cx) {
	seg_ent_t* seg = &cx->seg[cx->curr_ifunc];
	icode_t* ir = &((icode_t*)seg->data)[cx->i];

	#define UPD(r) (cx->reg_lifetimes[r] = cx->i)

	switch (ir->op) {
	case IR_SRESV:
		sresv(cx, ir->regs[0], ir->regs[1]);
	case IR_SYSCALL: case IR_SETARG: case IR_GETARG:
	case IR_INT: case IR_FLOAT: case IR_GETLBL: case IR_SEG:
		UPD(ir->dst);
		break;

	case IR_ENTER: case IR_BEGCALL: case IR_LBL: break;

	case IR_CALL: UPD(ir->dst); UPD(ir->regs[0]); break;

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
b8 ireg_movable(amd64_ctx_t* cx, u32 reg) {
	amd64_ireg_t* ireg = &cx->reg_map[reg];
	return (reg_flags[ireg->mreg] & REG_ALLOCATABLE) && cx->reg_lifetimes[reg] == cx->i && cx->reg_allocated[ireg->mreg] == reg;
}

static
void ireg_end(amd64_ctx_t* cx, u32 reg) {
	amd64_ireg_t* ireg = &cx->reg_map[reg];

	if (ireg_reg_any(ireg) && ireg_movable(cx, reg) && !reg_free(cx, ireg->mreg))
		lt_printf(A_BOLD"m-cs '%S': "A_MAGENTA"internal compiler error"A_RESET": invalid free of %S (r%ud)\n",
						cx->seg[cx->curr_func].name, reg_names[ireg->mreg][VARG_64], reg);
}

static
void x64_mcopy(amd64_ctx_t* cx, amd64_ireg_t dst, amd64_ireg_t src, usz bytes) {
	if (!bytes)
		return;

	amd64_ireg_t tmp = XREG(reg_scratch(cx, 0), ISZ_64);
	dst.type |= IREG_REF;
	src.type |= IREG_REF;
	dst.size = ISZ_64;
	src.size = ISZ_64;

	usz size = 8;

	while (size) {
		while (bytes >= size) {
			x64_mov(cx, tmp, src);
			x64_mov(cx, dst, tmp);
			dst.disp += src.size;
			src.disp += src.size;
			bytes -= src.size;
		}
		size >>= 1;
		dst.size = size;
		src.size = size;
		tmp.size = size;
	}

// 	x64_mov(cx, XREG(REG_DI, ISZ_64), dst);
// 	x64_mov(cx, XREG(REG_SI, ISZ_64), src);
// 	x64_mov(cx, XREG(REG_C, ISZ_64), XIMMI(bytes));

// 	amd64_instr_t mi;
// 	memset(&mi, 0, sizeof(mi));
// 	mi.op = X64_MOVSB;
// 	mi.var = 0;
// 	mi.prefix[0] = 0xF3;
// 	emit(cx, mi);
}

static
amd64_ireg_t* init_new_reg(amd64_ctx_t* cx, u32 dst, u32 src, usz size) {
	amd64_ireg_t* ireg = &cx->reg_map[src];
	amd64_ireg_t* dst_ireg = &cx->reg_map[dst];

	if (ireg_reg_any(ireg) && ireg_movable(cx, src)) {
		ireg_move(cx, dst, src);
		dst_ireg->size = size;
		return dst_ireg;
	}

	*dst_ireg = XREG(reg_alloc(cx, dst), size);
	x64_mov(cx, *dst_ireg, *ireg);
	return dst_ireg;
}

static
void convert_icode(amd64_ctx_t* cx) {
	seg_ent_t* seg = &cx->seg[cx->curr_ifunc];

	usz i = cx->i;

	icode_t* ir = &((icode_t*)seg->data)[i];
	amd64_instr_t mi;
	memset(&mi, 0, sizeof(mi));

	amd64_ireg_t* dst = &cx->reg_map[ir->dst];
	amd64_ireg_t* reg0 = &cx->reg_map[ir->regs[0]];
	amd64_ireg_t* reg1 = &cx->reg_map[ir->regs[1]];

	switch (ir->op) {
	case IR_INT: *dst = XIMMI(ir->uint_val); break;
	case IR_SEG: *dst = XSEG(ir->uint_val); break;
	case IR_GETLBL: *dst = XLBL(ir->uint_val); break;

	case IR_SRESV:
		*dst = XREG(REG_SP, ISZ_64);
		dst->disp = sresv(cx, ir->regs[0], ir->regs[1]);
		break;

	case IR_LOAD: {
		*dst = XREG(reg_alloc(cx, ir->dst), ir->size);

		amd64_ireg_t tmp_src = *reg0;
		LT_ASSERT((!(reg0->type & IREG_REF)));
		tmp_src.type |= IREG_REF;
		tmp_src.size = ir->size;

		x64_mov(cx, *dst, tmp_src);
	}	break;

	case IR_STOR: {
		amd64_ireg_t tmp_dst = *dst;

		if (tmp_dst.type == IREG_IMM || (tmp_dst.type & IREG_REF)) {
			u32 tmpreg = reg_scratch(cx, 0);
			x64_mov(cx, XREG(tmpreg, ISZ_64), *dst);
			tmp_dst = XREG(tmpreg, ISZ_64);
		}
		tmp_dst.type |= IREG_REF;
		tmp_dst.size = ir->size;

		amd64_ireg_t src = *reg0;
		src.size = ir->size;

		x64_mov(cx, tmp_dst, src);
	}	break;

	#define TOU(s) { \
		init_new_reg(cx, ir->dst, ir->regs[0], ir->size); \
		dst->size = (s); \
		x64_movzx(cx, *dst, *reg0); \
	}

	#define TOI(s) { \
		init_new_reg(cx, ir->dst, ir->regs[0], ir->size); \
		dst->size = (s); \
		x64_movsx(cx, *dst, *reg0); \
	}

	case IR_TOU8: TOU(ISZ_8) break;
	case IR_TOU16: TOU(ISZ_16) break;
	case IR_TOU32: TOU(ISZ_32) break;
	case IR_TOU64: TOU(ISZ_64) break;

	case IR_TOI8: TOI(ISZ_8) break;
	case IR_TOI16: TOI(ISZ_16) break;
	case IR_TOI32: TOI(ISZ_32) break;
	case IR_TOI64: TOI(ISZ_64) break;

#define GENERIC2(x) { \
	amd64_ireg_t args[2] = {*init_new_reg(cx, ir->dst, ir->regs[0], ir->size), *reg1}; \
	emit_instr(cx, (x), 2, args); \
}

	case IR_ADD: GENERIC2(X64_ADD); break;
	case IR_SUB: GENERIC2(X64_SUB); break;
	case IR_AND: GENERIC2(X64_AND); break;
	case IR_OR: GENERIC2(X64_OR); break;
	case IR_XOR: GENERIC2(X64_XOR); break;

#define SHIFT(x) { \
	init_new_reg(cx, ir->dst, ir->regs[0], ISZ_8); \
	amd64_ireg_t reg1_tmp = *reg1; \
	reg1_tmp.size = ISZ_8; \
	x64_mov(cx, XREG(REG_C, ISZ_8), reg1_tmp); \
	dst->size = ir->size; \
	emit_instr(cx, (x), 1, dst); \
}

	case IR_USHL: SHIFT(X64_SHL); break;
	case IR_ISHL: SHIFT(X64_SAL); break;
	case IR_USHR: SHIFT(X64_SHR); break;
	case IR_ISHR: SHIFT(X64_SAR); break;

#define GENERIC3(x, res_reg) { \
	x64_mov(cx, XREG(REG_A, ir->size), *reg0); \
	amd64_ireg_t arg = *reg1; \
	if (arg.type == IREG_IMM || arg.type == IREG_SEG || arg.type == IREG_LBL) { \
		arg = XREG(REG_C, ir->size); \
		x64_mov(cx, arg, *reg1); \
	} \
	zero_reg(cx, REG_D); \
	emit_instr(cx, (x), 1, &arg); \
	\
	*dst = XREG(reg_alloc(cx, ir->dst), ir->size); \
	x64_mov(cx, *dst, XREG(res_reg, ir->size)); \
}

	case IR_IREM: GENERIC3(X64_IDIV, REG_D); break;
	case IR_UREM: GENERIC3(X64_DIV, REG_D); break;
	case IR_IDIV: GENERIC3(X64_IDIV, REG_A); break;
	case IR_UDIV: GENERIC3(X64_DIV, REG_A); break;

	case IR_IMUL: GENERIC3(X64_IMUL, REG_A); break;
	case IR_UMUL: GENERIC3(X64_MUL, REG_A); break;

#define GENERIC1(x) emit_instr(cx, (x), 1, init_new_reg(cx, ir->dst, ir->regs[0], ir->size))

	case IR_NEG: GENERIC1(X64_NEG); break;
	case IR_NOT: GENERIC1(X64_NOT); break;
	case IR_INC: GENERIC1(X64_INC); break;
	case IR_DEC: GENERIC1(X64_DEC); break;

#define GENERIC4(x) { \
	*dst = XREG(reg_alloc(cx, ir->dst), ISZ_8); \
	zero_reg(cx, dst->mreg); \
	amd64_ireg_t args[2] = { *reg0, *reg1 }; \
	emit_instr(cx, X64_CMP, 2, args); \
	emit_instr(cx, (x), 1, dst); \
	dst->size = ISZ_8; \
}

	case IR_CSETG: GENERIC4(X64_SETG); break;
	case IR_CSETGE: GENERIC4(X64_SETGE); break;
	case IR_CSETL: GENERIC4(X64_SETL); break;
	case IR_CSETLE: GENERIC4(X64_SETLE); break;
	case IR_CSETA: GENERIC4(X64_SETA); break;
	case IR_CSETAE: GENERIC4(X64_SETAE); break;
	case IR_CSETB: GENERIC4(X64_SETB); break;
	case IR_CSETBE: GENERIC4(X64_SETBE); break;
	case IR_CSETE: GENERIC4(X64_SETE); break;
	case IR_CSETNE: GENERIC4(X64_SETNE); break;

	case IR_CSETZ: {
		*dst = XREG(reg_alloc(cx, ir->dst), ISZ_8);
		zero_reg(cx, dst->mreg);
		amd64_ireg_t args[2] = { *reg0, *reg0 };
		emit_instr(cx, X64_TEST, 2, args);
		emit_instr(cx, X64_SETZ, 1, dst);
		dst->size = ISZ_8;
	}	break;

	case IR_CSETNZ: {
		*dst = XREG(reg_alloc(cx, ir->dst), ISZ_8);
		zero_reg(cx, dst->mreg);
		amd64_ireg_t args[2] = { *reg0, *reg0 };
		emit_instr(cx, X64_TEST, 2, args);
		emit_instr(cx, X64_SETNZ, 1, dst);
		dst->size = ISZ_8;
	}	break;

#define GENERIC5(x) { \
	amd64_ireg_t args[2] = { *reg0, *reg1 }; \
	emit_instr(cx, X64_CMP, 2, args); \
	emit_instr(cx, (x), 1, dst); \
}

	case IR_CJMPG: GENERIC5(X64_JG); break;
	case IR_CJMPGE: GENERIC5(X64_JGE); break;
	case IR_CJMPL: GENERIC5(X64_JL); break;
	case IR_CJMPLE: GENERIC5(X64_JLE); break;
	case IR_CJMPA: GENERIC5(X64_JA); break;
	case IR_CJMPAE: GENERIC5(X64_JAE); break;
	case IR_CJMPB: GENERIC5(X64_JB); break;
	case IR_CJMPBE: GENERIC5(X64_JBE); break;
	case IR_CJMPE: GENERIC5(X64_JE); break;
	case IR_CJMPNE: GENERIC5(X64_JNE); break;

	case IR_CJMPZ: {
		amd64_ireg_t args[2] = { *reg0, *reg0 };
		emit_instr(cx, X64_TEST, 2, args);
		emit_instr(cx, X64_JZ, 1, dst);
	}	break;

	case IR_CJMPNZ: {
		amd64_ireg_t args[2] = { *reg0, *reg0 };
		emit_instr(cx, X64_TEST, 2, args);
		emit_instr(cx, X64_JNZ, 1, dst);
	}	break;

	case IR_JMP: {
		emit_instr(cx, X64_JMP, 1, dst);
	}	break;

	case IR_ENTER:
		reg_push_caller_owned(cx);
		reg_push64(cx, cconvs[CCONV_SYSV].regs[0]);
		if (cx->frame_size) {
			amd64_ireg_t args[2] = { XREG(REG_SP, ISZ_64), XIMMI(cx->frame_size) };
			emit_instr(cx, X64_SUB, 2, args);
		}
		break;

	case IR_GETARG: {
		LT_ASSERT(ir->size);
		if (ir->size <= 8) {
			amd64_ireg_t dst_tmp = *dst;
			LT_ASSERT(!(dst->type & IREG_REF));
			dst_tmp.type |= IREG_REF;
			dst_tmp.size = ir->size;
			x64_mov(cx, dst_tmp, XREG(cconvs[CCONV_SYSV].regs[cx->getarg_offs + ir->regs[0]], ir->size));
		}
		else
			x64_mcopy(cx, *dst, XREG(cconvs[CCONV_SYSV].regs[cx->getarg_offs + ir->regs[0]], ISZ_64), ir->size);
	}	break;

	case IR_BEGCALL: {
		if (ir->size > 8)
			++cx->arg_num;
		cx->cconv = ir->dst;
	}	break;

	case IR_SETARG: {
		call_conv_t* cconv = &cconvs[cx->cconv];
		LT_ASSERT(cx->arg_num < cconv->reg_count);
		u8 reg = cconv->regs[cx->arg_num++];
		u8 size = ir->size;
		if (size > ISZ_64)
			size = ISZ_64;
		x64_mov(cx, XREG(reg, size), *dst);
	}	break;

	case IR_SYSCALL:
		mi.op = X64_SYSCALL;
		mi.var = 0;
		emit(cx, mi);

		*dst = XREG(reg_alloc(cx, ir->dst), ISZ_64);
		x64_mov(cx, *dst, XREG(REG_A, ISZ_64));

		cx->arg_num = 0;
		break;

	case IR_CALL:
		if (ir->size > 8)
			x64_mov(cx, XREG(cconvs[cx->cconv].regs[0], ISZ_64), *reg0);

		emit_instr(cx, X64_CALL, 1, dst);
		cx->arg_num = 0;

		usz* lifetime = &cx->reg_lifetimes[ir->regs[0]];
		if (*lifetime == i) {
			*lifetime = 0;
			break;
		}

		if (ir->size <= 8 && ir->size) {
 			*reg0 = XREG(reg_alloc(cx, ir->regs[0]), ir->size);
			x64_mov(cx, *reg0, XREG(REG_A, ir->size));
 		}
		break;

	case IR_RET:
		if (ir->dst) {
			LT_ASSERT(ir->size);
			usz size = ir->size;
			if (size > 8)
				size = 8;
			x64_mov(cx, XREG(REG_A, size), *dst);
		}
		if (cx->frame_size) {
			amd64_ireg_t args[2] = { XREG(REG_SP, ISZ_64), XIMMI(cx->frame_size) };
			emit_instr(cx, X64_ADD, 2, args);
		}
		reg_pop64(cx, cconvs[CCONV_SYSV].regs[0]);
		if (ir->size > 8)
			x64_mcopy(cx, XREG(cconvs[CCONV_SYSV].regs[0], ISZ_64), XREG(REG_A, ISZ_64), ir->size);
		reg_pop_caller_owned(cx);

		mi.op = X64_RET;
		emit(cx, mi);
		break;

	case IR_MCOPY:
		x64_mcopy(cx, *dst, *reg0, ir->size);
		break;

	case IR_LBL:
		mi.op = X64_IR_LBL;
		mi.imm.imm = ir->uint_val;
		emit(cx, mi);
		break;

	default:
		lt_printf("Unhandled intermediate instruction %S\n", icode_op_str(ir->op));
		LT_ASSERT_NOT_REACHED();
	}

	u8 regs = ir_get_regs(ir);
	if (regs & IR_REG_DST)
		ireg_end(cx, ir->dst);
	if (regs & IR_REG_R0)
		ireg_end(cx, ir->regs[0]);
	if (regs & IR_REG_R1)
		ireg_end(cx, ir->regs[1]);
}

void amd64_gen(amd64_ctx_t* cx) {
	cx->reg_map = lt_arena_reserve(cx->arena, sizeof(amd64_ireg_t) * 2048);
	cx->reg_lifetimes = lt_arena_reserve(cx->arena, sizeof(usz) * 2048);

	usz seg_count = cx->seg_count;

	for (usz i = 0; i < seg_count; ++i) {
		seg_ent_t* cs = &cx->seg[i];

		if (cs->stype != SEG_ICODE)
			continue;

		memset(cx->reg_allocated, 0, sizeof(cx->reg_allocated));

		cx->curr_ifunc = i;
		cx->curr_func = new_mcode_seg(cx, cs->type, cs->name, i);
		cx->stack_top = 0;
		cx->arg_num = 0;
		cx->getarg_offs = 0;
		if (type_bytes(cs->type->base) > 8)
			cx->getarg_offs = 1;
		cx->lbl = NULL;

		seg_ent_t* ms = &cx->seg[cx->curr_func];

		for (cx->i = 0; cx->i < cs->size; ++cx->i)
			prepass_icode(cx);

		cx->lbl_it = cx->lbl;

		cx->frame_size = sresv(cx, 0, 16);
		cx->stack_top = 0;

		for (cx->i = 0; cx->i < cs->size; ++cx->i)
			convert_icode(cx);

#ifdef LT_DEBUG
		for (usz i = 0 ; i < AMD64_REG_COUNT; ++i)
			if (cx->reg_allocated[i])
				lt_printf(A_BOLD"m-cs '%S': "A_MAGENTA"internal compiler error"A_RESET": leaked register %S (r%ud)\n",
						cs->name, reg_names[i][VARG_64], cx->reg_allocated[i]);
#endif

		ms->lbl = cx->lbl;
	}
}

static
void print_modrm(u8 mod, u8 rm, u8 size, u32 disp) {
	switch (mod) {
		case MOD_DREG: lt_printf("[%S]", reg_names[rm][VARG_64]); break;
		case MOD_DSP8: lt_printf("[%S + %id]", reg_names[rm][VARG_64], disp); break;
		case MOD_DSP32: lt_printf("[%S + %id]", reg_names[rm][VARG_64], disp); break;
		case MOD_REG: lt_printf("%S", reg_names[rm][size]); break;
	}
}

void amd64_print_seg(amd64_ctx_t* cx, usz i) {
	amd64_instr_t* mcode = cx->seg[i].data;
	usz mcode_count = cx->seg[i].size;

	for (usz i = 0; i < mcode_count; ++i) {
		lt_printf("\t(%uz)\t", i);
		amd64_print_instr(cx, &mcode[i]);
		lt_printc('\n');
	}
}

void amd64_print_instr(amd64_ctx_t* cx, amd64_instr_t* instr) {
	if (instr->op == X64_IR_LBL) {
		lt_printf("L%uq:", instr->imm.imm);
		return;
	}

	amd64_op_t* op = &ops[instr->op];
	amd64_var_t* var = &op->vars[instr->var];

	if (!op->var_count) {
		lt_printf("  %S", op->str);
		return;
	}

	lt_printf("  %S ",  op->str);

	u8 reg = instr->mrm.reg_rm & 0b1111;
	u8 rm = instr->mrm.reg_rm >> 4;

	for (usz i = 0; i < var->arg_count; ++i) {
		if (i)
			lt_printls(CLSTR(", "));

		u8 arg = var->args[i];
		u8 size = arg & VARG_SIZE_MASK;

		switch (arg & VARG_TYPE_MASK) {
		case VARG_REG: lt_printf("%S", reg_names[reg][size]); break;
		case VARG_MRM:
			if (instr->disp_flags & MI_SEG)
				lt_printf("[<%S+%ud>]", cx->seg[instr->mrm.disp].name, instr->mrm.disp2);
			else if (instr->disp_flags & MI_LBL)
				lt_printf("[(%id+%ud)]", instr->mrm.disp, instr->mrm.disp2);
			else
				print_modrm(instr->mrm.mod, rm, size, instr->mrm.disp);
			break;
		case VARG_IMM:
			if (instr->imm_flags & MI_SEG)
				lt_printf("<%S+%ud>", cx->seg[instr->imm.index].name, instr->imm.disp);
			else if (instr->imm_flags & MI_LBL)
				lt_printf("(%id+%ud)", instr->imm.index, instr->imm.disp);
			else
				lt_printf("0x%hq", instr->imm.imm);
			break;
		}
	}
}

