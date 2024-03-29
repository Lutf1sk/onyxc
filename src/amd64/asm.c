#include "amd64.h"
#include "common.h"
#include "ops.h"
#include "regs.h"
#include "lbl.h"

#include "../segment.h"

#include <lt/math.h>
#include <lt/align.h>
#include <lt/mem.h>
#include <lt/io.h>
#include <lt/str.h>

typedef
struct asm_ctx {
	usz bin_size, bin_asize;
	u8* bin_data;
	usz base_addr;
	amd64_ctx_t* amd64_cx;
	seg_ent_t* seg;
	fwd_ref_t* refs;
	lt_arena_t* arena;
} asm_ctx_t;

static
usz make_space(asm_ctx_t* cx, usz size, usz align) {
	if (!align)
		align = 1;

	usz offs = lt_align_fwd(cx->bin_size, align);
	cx->bin_size = offs + size;
	while (cx->bin_size > cx->bin_asize) {
		cx->bin_asize <<= 1;
		cx->bin_data = realloc(cx->bin_data, cx->bin_asize);
		LT_ASSERT(cx->bin_data);
	}
	return offs;
}

static
usz write(asm_ctx_t* cx, void* mem, usz size, usz align) {
	usz offs = make_space(cx, size, align);
	void* addr = cx->bin_data + offs;
	memcpy(addr, mem, size);
	return offs;
}

static
void resolve_lbls(asm_ctx_t* cx, amd64_lbl_t** cx_lbl, u32 i) {
	amd64_lbl_t* lbl = find_lbl(*cx_lbl, i);
	if (!lbl)
		lbl = new_lbl(cx->arena, cx_lbl, i);

	lbl->m_i = cx->bin_size;
	for (usz i = 0; i < lbl->ref_count; ++i) {
		void* ptr = cx->bin_data + lbl->refs[i];

		u64 val;
		memcpy(&val, ptr, sizeof(u32));
		val += lbl->m_i;
		memcpy(ptr, &val, sizeof(u32));
	}
}

static
void write_lbl(asm_ctx_t* cx, amd64_lbl_t** cx_lbl, usz offs, u64* imm_val) {
	u64 lbl_i = *imm_val;

	amd64_lbl_t* lbl = find_lbl(*cx_lbl, lbl_i);
	if (!lbl) {
		lbl = new_lbl(cx->arena, cx_lbl, lbl_i);
		lbl->m_i = 0;
	}

	if (lbl->m_i)
		*imm_val = cx->base_addr + lbl->m_i;
	else {
		if (!lbl->ref_count)
			lbl->refs = lt_amalloc(cx->arena, sizeof(u32) * 32); // !! Terrible, horrible stuff. Fix this
		*imm_val = cx->base_addr;
		lbl->refs[lbl->ref_count++] = offs;
	}
}


static
void write_seg(asm_ctx_t* cx, usz offs, usz imm_bytes, u64* imm_val, u32 index) {
	*imm_val = cx->amd64_cx->segtab->seg[index].load_at;
	if (!*imm_val) {
		fwd_ref_t* new_ref = lt_amalloc(cx->arena, sizeof(fwd_ref_t));
		new_ref->offs = offs;
		new_ref->seg = index;
		new_ref->next = cx->refs;
		new_ref->size = imm_bytes;

		cx->refs = new_ref;
	}
}

static
usz assemble_func(asm_ctx_t* cx, seg_ent_t* seg) {
	cx->seg = seg;
	usz load_addr = cx->base_addr + make_space(cx, 0, 16);

// 	lt_printf("%S: 0x%hz\n", seg->name, load_addr);
	seg->load_at = load_addr;

	cx->amd64_cx->lbl_it = cx->amd64_cx->lbl = NULL;

	amd64_instr_t* instrs = seg->mcode_data;
	usz instr_count = seg->mcode_count;

	for (usz i = 0; i < instr_count; ++i) {
		u8 instr[16], *it = instr;

		amd64_instr_t* mi = &instrs[i];
		if (mi->op == X64_IR_LBL) {
			resolve_lbls(cx, &cx->amd64_cx->lbl, mi->imm.index);
			continue;
		}

		for (usz i = 0; mi->prefix[i] && i < sizeof(mi->prefix); ++i)
			*it++ = mi->prefix[i];

		amd64_op_t* op = &ops[mi->op];
		amd64_var_t* var = &op->vars[mi->var];
		u8* var_op = var->instr;

		u8 reg = mi->mrm.reg_rm & 0b111;
		u8 rm = (mi->mrm.reg_rm >> 4) & 0b111;

		b8 oi = var->flags & VFLAG_REGOP;
		b8 modrm = 0;
		b8 imm = 0;
		u8 imm_size = 0;
		u8 imm_bytes = 0;
		u8 imm_idx;
		b8 rex = 0;
		for (usz i = 0; i < var->arg_count; ++i) {
			u8 varg = var->args[i] & VARG_TYPE_MASK;
			u8 vsize = var->args[i] & VARG_SIZE_MASK;
			if (varg == VARG_IMM) {
				imm = 1;
				imm_size = vsize;
				imm_bytes = 1 << imm_size;
				imm_idx = i;
			}
			else if (varg == VARG_MRM) {
				modrm = 1;
				if ((rm & 0b100) && vsize == VARG_8)
					rex = 1;
			}
			else if (varg == VARG_REG) {
				if ((reg & 0b100) && vsize == VARG_8)
					rex = 1;
			}
		}

		if (var->flags & VFLAG_OPSIZE)
			*it++ = 0x66;

		u8 rex_w = !!(var->flags & VFLAG_REX_W);
		u8 reg_rex_bit = !!(mi->mrm.reg_rm & REG_REX_BIT);
		u8 rm_rex_bit = !!(mi->mrm.reg_rm & (REG_REX_BIT << 4));
		if (rex_w || reg_rex_bit || rm_rex_bit || rex) {
			if (modrm)
				*it++ = REX(rex_w, reg_rex_bit, 0, rm_rex_bit);
			else if (oi)
				*it++ = REX(rex_w, 0, 0, reg_rex_bit);
		}

		u8 ext = !!(var->flags & VFLAG_OP_EXT);
		usz var_i = ext;
		u8 var_b = var_op[var_i++];

		u8 oi_offs = 0;
		if (oi)
			oi_offs += reg;

		if (var_b == 0x0F) {
			*it++ = var_b;
			*it++ = var_op[var_i++] + oi_offs;
		}
		else
			*it++ = var_b + oi_offs;

		// TODO: Ensure that displacement fits within 4 bytes

		if (modrm) {
			if (ext)
				reg = var_op[0];

			if (rm == REG_BP && mi->mrm.mod == MOD_DREG) {
				mi->mrm.mod = MOD_DSP8;
				mi->mrm.disp = 0;
			}

			u64 disp_val = mi->mrm.disp;
			usz disp_bytes = 0;

			if (mi->disp_flags & MI_IP_DISP) {
				*it++ = MODRM(0, reg, REG_BP);

				disp_bytes = 4;
				u64 disp_offs = cx->bin_size + (it - instr);

				if (mi->disp_flags & MI_LBL) {
					write_lbl(cx, &cx->amd64_cx->lbl, disp_offs, &disp_val);
					disp_val -= cx->base_addr + disp_offs + disp_bytes + imm_bytes;
					disp_val += mi->mrm.disp2;
				}
				if (mi->disp_flags & MI_SEG) {
					write_seg(cx, disp_offs, disp_bytes, &disp_val, mi->mrm.disp);
					disp_val -= cx->base_addr + disp_offs + disp_bytes + imm_bytes;
					disp_val += mi->mrm.disp2;
				}
			}
			else if (mi->disp_flags & MI_DISP_ONLY) {
				*it++ = MODRM(0, reg, REG_SP);
				*it++ = SIB(SIB_S1, REG_SP, REG_BP);

				disp_bytes = 4;
				u64 disp_offs = cx->bin_size + (it - instr);

				if (mi->disp_flags & MI_LBL) {
					write_lbl(cx, &cx->amd64_cx->lbl, disp_offs, &disp_val);
					disp_val += mi->mrm.disp2;
				}
				if (mi->disp_flags & MI_SEG) {
					write_seg(cx, disp_offs, disp_bytes, &disp_val, mi->mrm.disp);
					disp_val += mi->mrm.disp2;
				}
			}
			else {
				*it++ = MODRM(mi->mrm.mod, reg, rm);
				if (mi->mrm.mod != MOD_REG) {
					if (rm == REG_SP)
						*it++ = SIB(SIB_S1, REG_SP, REG_SP);
				}
				switch (mi->mrm.mod) {
				case MOD_DSP8: disp_bytes = 1; break;
				case MOD_DSP32: disp_bytes = 4; break;
				}
			}

			memcpy(it, &disp_val, disp_bytes);
			it += disp_bytes;
		}

		if (imm) {
			u64 imm_val = mi->imm.imm;
			u64 imm_offs = cx->bin_size + (it - instr);

			if (mi->imm_flags & MI_LBL) {
				write_lbl(cx, &cx->amd64_cx->lbl, imm_offs, &imm_val);
				imm_val += mi->imm.disp;
			}
			if (mi->imm_flags & MI_SEG) {
				write_seg(cx, imm_offs, imm_bytes, &imm_val, mi->imm.index);
				imm_val += mi->imm.disp;
			}
			if (var->args[imm_idx] & VARG_REL)
				imm_val -= cx->base_addr + imm_offs + imm_bytes;

			memcpy(it, &imm_val, imm_bytes);
			it += imm_bytes;
		}

		write(cx, instr, it - instr, 0);
	}

	return load_addr;
}

void* amd64_link_program(amd64_ctx_t* cx, usz base_addr, usz* out_size) {
	asm_ctx_t asm_cx;
	asm_cx.bin_size = 0;
	asm_cx.bin_asize = 1024;
	asm_cx.bin_data = malloc(asm_cx.bin_asize);
	asm_cx.base_addr = base_addr;
	asm_cx.amd64_cx = cx;
	asm_cx.refs = NULL;
	asm_cx.arena = cx->arena;
	LT_ASSERT(asm_cx.bin_data);

	for (usz i = 0; i < cx->segtab->count; ++i) {
		seg_ent_t* seg = &cx->segtab->seg[i];
		if (seg->stype == SEG_CODE)
			assemble_func(&asm_cx, seg);
		else if (seg->stype == SEG_DATA) {
			usz seg_offs = write(&asm_cx, seg->data, seg->size, 16);
			seg->load_at = base_addr + seg_offs;
// 			lt_printf("%S: 0x%hz\n", seg->name, seg->load_at);

			fwd_ref_t* it = seg->ref;
			if (it) {
				for (;;) {
					it->offs += seg_offs;
					if (!it->next) {
						it->next = asm_cx.refs;
						break;
					}
					it = it->next;
				}

				asm_cx.refs = seg->ref;
			}
		}
	}

	fwd_ref_t* it = asm_cx.refs;
	while (it) {
		u64 val = 0;
		memcpy(&val, asm_cx.bin_data + it->offs, it->size);
		val += cx->segtab->seg[it->seg].load_at;
		memcpy(asm_cx.bin_data + it->offs, &val, it->size);

		it = it->next;
	}

	*out_size = asm_cx.bin_size;
	return asm_cx.bin_data;
}

#include <lt/asm.h>

static
void disasm(void* data, usz len) {
	u8 str[256];
	u8* str_it = str;
	lt_instr_stream_t s = lt_instr_stream_create((lt_io_callback_t)lt_str_io_callb, &str_it, data, len);
	for (;;) {
		usz offs = s.it - (u8*)data;
		usz len = lt_x64_disasm_instr(&s);
		if (!len) {
			if (s.it == s.end)
				return;
			lt_printf("%hz:\t??\n", offs);
			continue;
		}

		lt_printf("%hz:\t%S\n", offs, LSTR(str, len));
		str_it = str;
	}
}

void* amd64_jit_link_segment(amd64_ctx_t* cx, usz i) {
	seg_ent_t* seg = &cx->segtab->seg[i];
	if (seg->stype == SEG_CODE) {
		if (!seg->jit_data)
			amd64_jit_assemble_func(cx, i);
		else
			return seg->jit_data;
	}
	else {
		if (seg->jit_data)
			return seg->jit_data;
		seg->jit_data = lt_amalloc(cx->arena, seg->size);
		memcpy(seg->jit_data, seg->data, seg->size);
	}

	fwd_ref_t* it = seg->ref;
	while (it) {
		seg_ent_t* it_seg = &cx->segtab->seg[it->seg];

		amd64_jit_link_segment(cx, it->seg);

		u64 val = 0;
		memcpy(&val, seg->jit_data + it->offs, it->size);
		val += (usz)it_seg->jit_data;
		memcpy(seg->jit_data + it->offs, &val, it->size);

		it = it->next;
	}

// 	if (seg->stype == SEG_CODE) {
// 		lt_printf("JIT '%S' 0x%hz\n", seg->name, seg->jit_data);
// 		disasm(seg->jit_data, seg->size);
// 	}

	return seg->jit_data;
}

void amd64_jit_assemble_func(amd64_ctx_t* cx, usz seg_i) {
	seg_ent_t* seg = &cx->segtab->seg[seg_i];
	if (seg->jit_data) // !!
		return;

	if (!seg->mcode_data)
		amd64_gen_func(cx, seg_i);

	asm_ctx_t asm_cx;
	asm_cx.bin_size = 0;
	asm_cx.bin_asize = 32 * lt_get_pagesize();
	asm_cx.bin_data = lt_vmalloc(asm_cx.bin_asize);
	asm_cx.base_addr = (usz)asm_cx.bin_data;
	asm_cx.amd64_cx = cx;
	asm_cx.refs = NULL;
	asm_cx.arena = cx->arena;
	LT_ASSERT(asm_cx.bin_data);

	assemble_func(&asm_cx, &cx->segtab->seg[seg_i]);

	seg->size = asm_cx.bin_size;
	seg->jit_data = asm_cx.bin_data;
	seg->ref = asm_cx.refs;
}

