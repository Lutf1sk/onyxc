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

	if (mi.imm_flags & MI_LBL)
		add_lbl_ref(cx, find_lbl(cx, mi.imm.index), ent->size);
	if (mi.disp_flags & MI_LBL)
		add_lbl_ref(cx, find_lbl(cx, mi.imm.index), -ent->size);

	return ent->size++;
}

static
usz immsz(amd64_ireg_t* ireg, u8 sign_ext) {
	LT_ASSERT(ireg->type == IREG_IMM);

	u64 imm = ireg->imm;
	if (sign_ext) {
		if (imm >= 0x80000000)
			return 8;
		else if (imm >= 0x8000)
			return 4;
		else if (imm >= 0x80)
			return 2;
		else
			return 1;
	}

	if (imm > 0xFFFFFFFF)
		return 8;
	else if (imm > 0xFFFF)
		return 4;
	else if (imm > 0xFF)
		return 2;
	else
		return 1;
}

void emit_instr(amd64_ctx_t* cx, u8 op_i, u8 arg_count, amd64_ireg_t* args_) {
	LT_ASSERT(arg_count <= 2);

	amd64_ireg_t args[2];
	memcpy(args, args_, sizeof(amd64_ireg_t) * arg_count);

	amd64_op_t* op = &ops[op_i];

	isz best_match = -1;
	usz min_score = -1;

	for (usz i = 0; i < op->var_count; ++i) {
		amd64_var_t* var = &op->vars[i];

		if (var->arg_count != arg_count)
			continue;

		usz score = 0;

		for (usz i = 0; i < arg_count; ++i) {
			amd64_ireg_t* ireg = &args[i];

			u8 type = ireg->type;
			u8 varg = var->args[i] & VARG_TYPE_MASK;
			u8 vbytes = 1 << (var->args[i] & VARG_SIZE_MASK);

			usz min_bytes = 0;
			if (type == IREG_IMM)
				min_bytes = immsz(ireg, var->args[i] & VARG_SGEXT);

			switch (type) {
			case IREG_REG:
				LT_ASSERT(ireg->size);
				if ((varg != VARG_MRM && varg != VARG_REG) || vbytes != ireg->size)
					goto next_var;
				break;

			case IREG_REG | IREG_REF:
				LT_ASSERT(ireg->size);
				if (varg != VARG_MRM || vbytes != ireg->size)
					goto next_var;
				break;

			case IREG_IMM | IREG_REF:
			case IREG_SEG | IREG_REF:
			case IREG_LBL | IREG_REF:
				LT_ASSERT(ireg->size);
				if (varg != VARG_MRM || vbytes != ireg->size)
					goto next_var;
				break;

			case IREG_LBL:
			case IREG_SEG:
				min_bytes = 4; // This assumes that no labels/segments have offsets greater than 2GiB
			case IREG_IMM:
				if (vbytes < min_bytes)
					goto next_var;
				if (varg == VARG_REG || varg == VARG_MRM)
					score++;
				else if (varg != VARG_IMM)
					goto next_var;
				break;
			}
		}

		if (score < min_score) {
			best_match = i;
			min_score = score;
		}
	next_var:
	}

	if (best_match < 0) {
		lt_printf("Invalid operands to '%S' ", op->str);
		for (usz i = 0; i < arg_count; ++i)
			lt_printf("(type:%ud,size:%ud)", args[i].type, args[i].size);
		lt_printc('\n');
		LT_ASSERT_NOT_REACHED();
		return;
	}

	amd64_var_t* var = &op->vars[best_match];

	amd64_instr_t mi;
	memset(&mi, 0, sizeof(mi));

#define WR_REG(r) do { \
	if (varg == VARG_REG) \
		mi.mrm.reg_rm |= (r); \
	else if (varg == VARG_MRM) { \
		mi.mrm.mod = MOD_REG; \
		mi.mrm.reg_rm |= (r) << 4; \
	} \
	else \
		LT_ASSERT_NOT_REACHED(); \
} while (0)


	for (usz i = 0; i < arg_count; ++i) {
		amd64_ireg_t* ireg = &args[i];

		u8 type = ireg->type;
		u8 varg = var->args[i] & VARG_TYPE_MASK;
		u8 vbytes = 1 << (var->args[i] & VARG_SIZE_MASK);

		switch (type) {
		case IREG_REG:
			if (ireg->disp) {
				ireg->type |= IREG_REF;
				amd64_ireg_t tmp = XREG(reg_scratch(cx, i), ireg->size);
				x64_lea(cx, tmp, *ireg);
				*ireg = tmp;
			}
			WR_REG(ireg->mreg);
			break;

		case IREG_REG | IREG_REF:
			mi.mrm.reg_rm |= ireg->mreg << 4;

			if (ireg->disp) {
				if (ireg->disp < 0x80)
					mi.mrm.mod = MOD_DSP8;
				else
					mi.mrm.mod = MOD_DSP32;
				mi.mrm.disp = ireg->disp;
			}
			else
				mi.mrm.mod = MOD_DREG;
			break;

		case IREG_IMM | IREG_REF:
			mi.disp_flags = MI_DISP_ONLY;
			mi.mrm.disp = ireg->imm + ireg->disp;
			break;

		case IREG_SEG | IREG_REF:
			mi.disp_flags = MI_SEG|MI_DISP_ONLY;
			mi.mrm.disp = ireg->seg;
			mi.mrm.disp2 = ireg->disp;
			break;

		case IREG_LBL | IREG_REF:
			mi.disp_flags = MI_LBL|MI_DISP_ONLY;
			mi.mrm.disp = ireg->lbl;
			mi.mrm.disp2 = ireg->disp;
			break;

		case IREG_SEG:
		case IREG_LBL:
		case IREG_IMM:
			if (varg == VARG_REG || varg == VARG_MRM) {
				u32 mreg = reg_scratch(cx, i);
				x64_mov(cx, XREG(mreg, vbytes), *ireg);
				WR_REG(mreg);
			}
			else if (varg == VARG_IMM) {
				if (type == IREG_LBL) {
					mi.imm_flags = MI_LBL;
					mi.imm.index = ireg->lbl;
					mi.imm.disp = ireg->disp;
				}
				else if (type == IREG_SEG) {
					mi.imm_flags = MI_SEG;
					mi.imm.index = ireg->seg;
					mi.imm.disp = ireg->disp;
				}
				else {
					mi.imm.imm = ireg->imm + ireg->disp;
				}
			}
			break;
		}
	}

	mi.op = op_i;
	mi.var = best_match;
	emit(cx, mi);
}

