#include "amd64.h"

#include "../elf.h"
#include "../segment.h"

#include <lt/align.h>
#include <lt/io.h>
#include <lt/str.h>

static
void fill_fh(lt_elf64_fh_t* fh) {
	memset(fh, 0, sizeof(lt_elf64_fh_t));

	fh->magic[0] = 0x7f;
	fh->magic[1] = 'E';
	fh->magic[2] = 'L';
	fh->magic[3] = 'F';

	fh->class = LT_ELFCLASS_64;
	fh->encoding = LT_ELFENC_LSB;
	fh->header_version = LT_ELF_VERSION_CURRENT;
	fh->osabi = LT_ELFOSABI_SYSV;
	fh->abi_version = 0;

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

#define ALIGN_BYTES 4096
static u8 align_buf[ALIGN_BYTES];

#define LOAD_ADDR 0x70000000

void amd64_write_elf64(amd64_ctx_t* cx, char* path) {
	lt_elf64_fh_t fh;
	fill_fh(&fh);
	fh.ph_count = 1;
	fh.ph_offset = sizeof(fh);

	usz bin_size = 0;
	void* bin_data = amd64_assemble_program(cx, LOAD_ADDR, &bin_size);

	for (usz i = 0; i < cx->segtab->count; ++i) {
		seg_ent_t* seg = &cx->segtab->seg[i];
		if (seg->stype == SEG_MCODE && lt_lstr_eq(seg->name, CLSTR("main")))
			fh.entry = seg->load_at;
	}

	if (!fh.entry)
		lt_ferrf("No 'main' function found\n");

	lt_elf64_ph_t ph;
	ph.type = LT_ELF_PH_TYPE_LOAD;
	ph.flags = LT_ELF_PH_X | LT_ELF_PH_R | LT_ELF_PH_W;
	ph.offset = ALIGN_BYTES; // !!
	ph.vaddr = LOAD_ADDR;
	ph.paddr = 0;
	ph.file_size = bin_size;
	ph.mem_size = bin_size;
	ph.alignment = ALIGN_BYTES;

	lt_file_t* f = lt_file_open(path, LT_FILE_W, LT_FILE_PERMIT_X, &cx->arena->interf);
	if (!f)
		lt_ferrf("Failed to open output file '%s'\n", path);

	lt_file_write(f, &fh, sizeof(fh));
	lt_file_write(f, &ph, sizeof(ph));
	lt_file_write(f, align_buf, ALIGN_BYTES - (sizeof(fh) + sizeof(ph))); // !!
	lt_file_write(f, bin_data, bin_size);

	lt_file_close(f, &cx->arena->interf);
}

