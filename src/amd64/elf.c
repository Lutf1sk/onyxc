#include "amd64.h"
#include "common.h"
#include "ops.h"
#include "regs.h"

#include "../elf.h"
#include "../segment.h"

#include <lt/align.h>
#include <lt/io.h>
#include <lt/str.h>

static
void fill_fh(lt_elf64_fh_t* fh) {
	fh->magic[0] = 0x7f;
	fh->magic[1] = 'E';
	fh->magic[2] = 'L';
	fh->magic[3] = 'F';

	fh->class = LT_ELFCLASS_64;
	fh->encoding = LT_ELFENC_LSB;
	fh->header_version = LT_ELF_VERSION_CURRENT;
	fh->osabi = LT_ELFOSABI_SYSV;
	fh->abi_version = 0;

	memset(fh->pad, 0, sizeof(fh->pad));

	fh->obj_type = LT_ELFTYPE_EXEC;
	fh->arch = LT_ELFARCH_AMD64;
	fh->version = 0;

	fh->entry = 0;
	fh->ph_offset = 0;
	fh->sh_offset = 0;

	fh->cpu_flags = 0;

	fh->fh_size = sizeof(lt_elf64_fh_t);

	fh->ph_size = sizeof(lt_elf64_ph_t);
	fh->ph_count = 0;
	fh->sh_size = sizeof(lt_elf64_sh_t);
	fh->sh_count = 0;

	fh->sh_strtab_index = 0;
}

typedef
struct elf_ctx {
	usz bin_size, bin_asize;
	u8* bin_data;
} elf_ctx_t;

static
usz make_space(elf_ctx_t* cx, usz size, usz align) {
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
usz write(elf_ctx_t* cx, void* mem, usz size, usz align) {
	usz offs = make_space(cx, size, align);
	void* addr = cx->bin_data + offs;
	memcpy(addr, mem, size);
	return offs;
}

#define LOAD_ADDR 0x70000000
#define ALIGN_BYTES 4096
static u8 align_buf[ALIGN_BYTES];

void amd64_write_elf64(amd64_ctx_t* cx, char* path) {
	lt_elf64_fh_t fh;
	fill_fh(&fh);
	fh.ph_count = 1;
	fh.ph_offset = sizeof(fh);

	elf_ctx_t elf_cx;
	elf_cx.bin_size = 0;
	elf_cx.bin_asize = 1024;
	elf_cx.bin_data = malloc(elf_cx.bin_asize);
	LT_ASSERT(elf_cx.bin_data);

	for (usz i = 0; i < cx->seg_count; ++i) {
		seg_ent_t* seg = &cx->seg[i];
		usz load_addr = LOAD_ADDR + make_space(&elf_cx, 0, 16);

		if (seg->stype == SEG_MCODE) {
			lt_printf("%S: 0x%hz\n", seg->name, load_addr);
			seg->load_at = load_addr;
			cx->seg[seg->origin].load_at = load_addr;

			amd64_instr_t* instrs = seg->data;
			usz instr_count = seg->size;

			if (lt_lstr_eq(seg->name, CLSTR("main")))
				fh.entry = load_addr;

			for (usz i = 0; i < instr_count; ++i) {
				u8 instr[16], *it = instr;

				amd64_instr_t* mi = &instrs[i];
				for (usz i = 0; mi->prefix[i] && i < sizeof(mi->prefix); ++i)
					*it++ = mi->prefix[i];

// 				if (mi->op == X64_IR_ENTER) {
// 					u8 frame_creat[] = {
// 						REX(1,0,0,0), 0x81, MODRM(MOD_REG,5,REG_SP), 0,0,0,0, // sub rsp, 0
// 					};
// 					*(u32*)&frame_creat[3] = seg->frame_size;
// 					write(&elf_cx, frame_creat, sizeof(frame_creat), 0);
// 					continue;
// 				}
// 				if (mi->op == X64_IR_RET) {
// 					u8 frame_destr[] = {
// 						REX(1,0,0,0), 0x81, MODRM(MOD_REG,0,REG_SP), 0,0,0,0, // add rsp, 0
// 						0xCB, // ret
// 					};
// 					*(u32*)&frame_destr[3] = seg->frame_size;
// 					lt_printf("frame_size %ud\n", seg->frame_size);
// 					write(&elf_cx, frame_destr, sizeof(frame_destr), 0);
// 					continue;
// 				}

				amd64_op_t* op = &ops[mi->op];
				amd64_var_t* var = &op->vars[mi->var];
				u8* var_op = var->instr;

				u8 reg = mi->reg_rm & 0b111;
				u8 rm = (mi->reg_rm >> 4) & 0b111;

				u8 rex_w = !!(var->flags & VFLAG_REX_W);
				u8 rex_r = !!(mi->reg_rm & REG_REX_BIT);
				u8 rex_b = !!(mi->reg_rm & (REG_REX_BIT << 4));

				if (rex_w || rex_r || rex_b)
					*it++ = REX(rex_w, rex_r, 0, rex_b);

				u8 ext = !!(var->flags & VFLAG_OP_EXT);

				for (usz i = ext; var_op[i] && i < sizeof(var->instr); ++i)
					*it++ = var_op[i];

				b8 modrm = 0;
				b8 imm = 0;
				u8 imm_flags = 0;
				u8 imm_size = 0;
				u8 imm_mi_flags = 0;
				for (usz i = 0; i < var->arg_count; ++i) {
					u8 varg = var->args[i] & VARG_TYPE_MASK;
					if (varg == VARG_IMM) {
						imm_size = var->args[i] & VARG_SIZE_MASK;
						imm = 1;
						imm_flags = var->args[i] & VARG_FLAG_MASK;
						imm_mi_flags = mi->flags[i];
					}
					else if (varg == VARG_MRM)
						modrm = 1;
				}

				if (modrm) {
					if (ext)
						reg = var_op[0];
					*it++ = MODRM(mi->mod, reg, rm);

					if (mi->mod != MOD_REG) {
						if (rm == REG_SP)
							*it++ = SIB(SIB_S1, REG_SP, REG_SP);
						LT_ASSERT(rm != REG_BP);

						switch (mi->mod) {
						case MOD_DSP8: *it++ = mi->disp; break;
						case MOD_DSP32:
							memcpy(it, &mi->disp, 4);
							it += 4;
							break;
						}
					}
				}

				if (imm) {
					u64 imm_val = mi->imm;
					u8 imm_bytes = 1 << imm_size;

					if (imm_mi_flags & MI_SEG) {
						imm_val = cx->seg[imm_val].load_at;
						LT_ASSERT(imm_val);
					}

					if (imm_flags & VARG_IP_REL) {
						imm_val -= LOAD_ADDR + elf_cx.bin_size;
						if (imm_flags & VARG_NIP)
							imm_val -= (it - instr) + imm_bytes;
					}

					memcpy(it, &imm_val, imm_bytes);
					it += imm_bytes;
				}

				write(&elf_cx, instr, it - instr, 0);
			}
			// TODO: Backpatching...
		}
		else if (seg->stype == SEG_DATA) {
			lt_printf("%S: 0x%hz\n", seg->name, load_addr);
			seg->load_at = load_addr;

			write(&elf_cx, seg->data, seg->size, 16);

			// TODO: Backpatching...
		}
	}

	if (!fh.entry)
		lt_ferrf("No 'main' function found\n");

	lt_elf64_ph_t ph;
	ph.type = LT_ELF_PH_TYPE_LOAD;
	ph.flags = LT_ELF_PH_X | LT_ELF_PH_R | LT_ELF_PH_W;
	ph.offset = ALIGN_BYTES; // !!
	ph.vaddr = LOAD_ADDR;
	ph.paddr = 0;
	ph.file_size = elf_cx.bin_size;
	ph.mem_size = elf_cx.bin_size;
	ph.alignment = ALIGN_BYTES;

	lt_file_t* f = lt_file_open(cx->arena, path, LT_FILE_W, LT_FILE_PERMIT_X);
	if (!f)
		lt_ferrf("Failed to open output file '%s'\n", path);

	lt_file_write(f, &fh, sizeof(fh));
	lt_file_write(f, &ph, sizeof(ph));
	lt_file_write(f, align_buf, ALIGN_BYTES - (sizeof(fh) + sizeof(ph))); // !!
	lt_file_write(f, elf_cx.bin_data, elf_cx.bin_size);

	free(elf_cx.bin_data);

	lt_file_close(f);
}

