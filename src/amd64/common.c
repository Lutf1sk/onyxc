#include <lt/io.h>

#include "common.h"
#include "amd64.h"
#include "lbl.h"
#include "ops.h"
#include "regs.h"

#include "../interm.h"
#include "../segment.h"

#define AMD64_BLOCK_SIZE 512
#define AMD64_BLOCK_MASK (AMD64_BLOCK_SIZE-1)

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

void ireg_copy(amd64_ctx_t* cx, u32 dst, u32 src) {
	amd64_ireg_t* dst_ireg = &cx->reg_map[dst];
	amd64_ireg_t* src_ireg = &cx->reg_map[src];

	if (ireg_reg_any(src_ireg)) {
		*dst_ireg = *src_ireg;
		return;
	}

	LT_ASSERT(cx->reg_lifetimes[src]);

	if (cx->reg_lifetimes[dst] > cx->reg_lifetimes[src])
		cx->reg_lifetimes[src] = 0;
	else
		cx->reg_lifetimes[dst] = 0;
	*dst_ireg = *src_ireg;
}

b8 ireg_reg_pure(amd64_ireg_t* ireg) {
	return ireg->type == IREG_REG && !ireg->disp;
}

b8 ireg_reg_displaced(amd64_ireg_t* ireg) {
	return ireg->type == IREG_REG && ireg->disp;
}

b8 ireg_reg_any(amd64_ireg_t* ireg) {
	return (ireg->type & ~IREG_REF) == IREG_REG;
}

#define SEGMENT_BLOCK_SIZE 512
#define SEGMENT_BLOCK_MASK (SEGMENT_BLOCK_SIZE-1)

usz new_mcode_seg(amd64_ctx_t* cx, type_t* type, lstr_t name, u32 origin) {
	if (!(cx->seg_count & SEGMENT_BLOCK_MASK))
		cx->seg = realloc(cx->seg, (cx->seg_count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	memset(&cx->seg[cx->seg_count], 0, sizeof(seg_ent_t));
	cx->seg[cx->seg_count].type = type;
	cx->seg[cx->seg_count].stype = SEG_MCODE;
	cx->seg[cx->seg_count].name = name;
	cx->seg[cx->seg_count].origin = origin;
	return cx->seg_count++;
}

usz emit(amd64_ctx_t* cx, amd64_instr_t mi) {
	LT_ASSERT(cx->curr_func != -1);

	seg_ent_t* ent = &cx->seg[cx->curr_func];
	if (!(ent->size & AMD64_BLOCK_MASK))
		ent->data = realloc(ent->data, (ent->size + AMD64_BLOCK_SIZE) * sizeof(amd64_instr_t));

	((amd64_instr_t*)ent->data)[ent->size] = mi;

	if ((mi.flags[0] & MI_LBL) || (mi.flags[1] & MI_LBL))
		add_lbl_ref(cx, find_lbl(cx, mi.imm), ent->size);

	return ent->size++;
}

void emit_instr(amd64_ctx_t* cx, u8 op_i, u8 arg_count, amd64_ireg_t* args_) {
	LT_ASSERT(arg_count <= 2);

	amd64_ireg_t args[2];
	memcpy(args, args_, sizeof(amd64_ireg_t) * arg_count);

	amd64_instr_t mi;
	amd64_op_t* op = &ops[op_i];

	if (arg_count >= 2) {
		if (ireg_reg_displaced(&args[1])) {
			u32 tmpreg = reg_scratch(cx, 1);
			amd64_ireg_t tmpireg = XREG(tmpreg, args[1].size);
			if (args[1].size > 1) {
				args[1].type |= IREG_REF;
				x64_lea(cx, tmpireg, args[1]);
			}
			else {
				u32 disp = args[1].disp;
				args[1].disp = 0;
				x64_mov(cx, tmpireg, args[1]);
				amd64_ireg_t add_args[2] = { tmpireg, XIMMI(disp) };
				emit_instr(cx, X64_ADD, 2, add_args);
			}
			args[1] = tmpireg;
		}
		if ((args[0].type & IREG_REF) && (args[1].type & IREG_REF)) {
			u32 tmpreg = reg_scratch(cx, 1);
			x64_mov(cx, XREG(tmpreg, args[1].size), args[1]);
			args[1] = XREG(tmpreg, args[1].size);
		}
	}

	for (usz i = 0; i < op->var_count; ++i) {
		amd64_var_t* var = &op->vars[i];

		if (var->arg_count != arg_count)
			continue;

		memset(&mi, 0, sizeof(mi));

		for (usz i = 0; i < arg_count; ++i) {
			amd64_ireg_t* ireg = &args[i];

			u8 type = ireg->type;
			u8 varg = var->args[i] & VARG_TYPE_MASK;
			u8 vbytes = 1 << (var->args[i] & VARG_SIZE_MASK);

			if (varg != VARG_IMM && vbytes != ireg->size)
				goto next_var;

			switch (type) {
			case IREG_REG:
				if (varg == VARG_MRM) {
					mi.mod = MOD_REG;
					mi.reg_rm |= ireg->mreg << 4;
				}
				else if (varg == VARG_REG)
					mi.reg_rm |= ireg->mreg;
				else
					goto next_var;
				break;

			case IREG_IMM:
				LT_ASSERT(!ireg->disp);
				if (varg != VARG_IMM)
					goto next_var;
				mi.imm = ireg->imm;
				break;

			case IREG_SEG:
				LT_ASSERT(!ireg->disp);
				if (varg != VARG_IMM)
					goto next_var;
				mi.flags[i] = MI_SEG;
				mi.imm = ireg->imm;
				break;

			case IREG_LBL:
				LT_ASSERT(!ireg->disp);
				if (varg != VARG_IMM)
					goto next_var;
				mi.flags[i] = MI_LBL;
				mi.imm = ireg->imm;
				break;

			case IREG_REG | IREG_REF:
				if (varg != VARG_MRM)
					goto next_var;
				mi.reg_rm |= ireg->mreg << 4;

				if (ireg->disp) {
					if (ireg->disp < 0x80)
						mi.mod = MOD_DSP8;
					else
						mi.mod = MOD_DSP32;
					mi.disp = ireg->disp;
				}
				else
					mi.mod = MOD_DREG;
				break;

			case IREG_IMM | IREG_REF:
				LT_ASSERT(!ireg->disp);
				if (varg != VARG_MRM)
					goto next_var;
				LT_ASSERT_NOT_REACHED();

			case IREG_SEG | IREG_REF:
				if (varg != VARG_MRM)
					goto next_var;
				mi.flags[i] = MI_SEG;
				LT_ASSERT_NOT_REACHED();

			case IREG_LBL | IREG_REF:
				LT_ASSERT_NOT_REACHED();
			}
		}

		mi.op = op_i;
		mi.var = i;
		emit(cx, mi);
		return;

	next_var:
	}

	lt_printf("Invalid operands to '%S' ", op->str);
	for (usz i = 0; i < arg_count; ++i)
		lt_printf("(type:%ud,size:%ud)", args[i].type, args[i].size);
	lt_printc('\n');
	*(u8*)0 = 0;
	LT_ASSERT_NOT_REACHED();
}

