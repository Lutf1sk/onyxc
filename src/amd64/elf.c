#include "amd64.h"
#include "common.h"
#include "ops.h"
#include "regs.h"

#include "../elf.h"
#include "../segment.h"

#include <lt/align.h>
#include <lt/io.h>

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

#define ENTRY_POINT 0x70000000
#define ALIGN_BYTES 4096
static u8 align_buf[ALIGN_BYTES];

void amd64_write_elf64(amd64_ctx_t* cx, char* path) {
	lt_elf64_fh_t fh;
	fill_fh(&fh);
	fh.entry = ENTRY_POINT;
	fh.ph_count = 1;
	fh.ph_offset = sizeof(fh);

	usz bin_size = 0;
	usz bin_asize = 1024;
	u8* bin_data = malloc(bin_asize);
	LT_ASSERT(bin_data);

	for (usz i = 0; i < cx->seg_count; ++i) {
		if (cx->seg[i].stype == SEG_MCODE) {
			amd64_instr_t* instrs = cx->seg[i].data;
			usz instr_count = cx->seg[i].size;

			for (usz i = 0; i < instr_count; ++i) {
				u8 instr[16], *it = instr;

				amd64_instr_t* mi = &instrs[i];
				for (usz i = 0; mi->prefix[i] && i < sizeof(mi->prefix); ++i)
					*it++ = mi->prefix[i];

				if (mi->op == X64_IR_ENTER)
					continue;
				if (mi->op == X64_IR_RET) {
					*it++ = 0xCB;
					goto write_instr;
				}

				amd64_op_t* op = &ops[mi->op];
				amd64_var_t* var = &op->vars[mi->var];
				u8* var_op = op->var_ops[mi->var];

				u8 reg = mi->reg_rm & 0b111;
				u8 rm = (mi->reg_rm >> 4) & 0b111;

				u8 rex_w = !!(var->sizes & VARG_REX_W);
				u8 rex_r = !!(mi->reg_rm & REG_REX_BIT);
				u8 rex_b = !!(mi->reg_rm & (REG_REX_BIT << 4));

				if (rex_w || rex_r || rex_b)
					*it++ = REX(rex_w, rex_r, 0, rex_b);

				u8 ext = !!(var->sizes & VARG_OP_EXT);

				for (usz i = ext; var_op[i] && i < sizeof(op->var_ops[mi->var]); ++i)
					*it++ = var_op[i];

				b8 imm = 0;
				u8 imm_size = 0;
				u8 arg_count = var->args & 0b11;
				for (usz i = 0; i < arg_count; ++i) {
					if (VARG_GET(var->args, i) == VMOD_IMM) {
						imm_size = VARG_GET(var->sizes, i);
						imm = 1;
						break;
					}
				}

				if (ext)
					reg = var_op[0];
				*it++ = MODRM(mi->mod, reg, rm);

				if (imm) {
					u8 imm_bytes = 1 << imm_size;
					memcpy(it, &mi->imm, imm_bytes); // TODO: Possible out-of-bounds read, since mi->imm is a u32
					it += imm_bytes;
				}

			write_instr:
				usz written = it - instr;
				LT_ASSERT(written);

				usz new_bin_size = bin_size + written;
				while (bin_asize > new_bin_size) {
					bin_asize >>= 1;
					bin_data = realloc(bin_data, new_bin_size);
					LT_ASSERT(bin_data);
				}
				memcpy(bin_data + bin_size, instr, written);
				bin_size += written;
			}

			// TODO: Backpatching...

		}
		else if (cx->seg[i].stype == SEG_DATA) {
			u8* seg_start = bin_data + bin_size;
			usz new_bin_size = bin_size + lt_align_fwd(cx->seg[i].size, 16);

			while (bin_asize > new_bin_size) {
				bin_asize >>= 1;
				bin_data = realloc(bin_data, new_bin_size);
				LT_ASSERT(bin_data);
			}

			// TODO: Backpatching...

			memcpy(seg_start, cx->seg[i].data, cx->seg[i].size);
			bin_size = new_bin_size;
		}
	}

	lt_elf64_ph_t ph;
	ph.type = LT_ELF_PH_TYPE_LOAD;
	ph.flags = LT_ELF_PH_X | LT_ELF_PH_R | LT_ELF_PH_W;
	ph.offset = ALIGN_BYTES; // !!
	ph.vaddr = ENTRY_POINT;
	ph.paddr = 0;
	ph.file_size = bin_size;
	ph.mem_size = bin_size;
	ph.alignment = ALIGN_BYTES;

	lt_file_t* f = lt_file_open(cx->arena, path, LT_FILE_W, LT_FILE_PERMIT_X);
	if (!f)
		lt_ferrf("Failed to open output file '%s'\n", path);

	lt_file_write(f, &fh, sizeof(fh));
	lt_file_write(f, &ph, sizeof(ph));
	lt_file_write(f, align_buf, ALIGN_BYTES - (sizeof(fh) + sizeof(ph))); // !!
	lt_file_write(f, bin_data, bin_size);

	free(bin_data);

	lt_file_close(f);
}

