#ifndef ELF_H
#define ELF_H 1

#include <lt/primitives.h>

#define LT_ELF_VERSION_CURRENT 1

typedef
enum lt_elf_class {
	LT_ELFCLASS_32 = 1,
	LT_ELFCLASS_64 = 2,
} lt_elf_class_t;

typedef
enum lt_elf_osabi {
	LT_ELFOSABI_SYSV = 0,
	LT_ELFOSABI_GNU = 3,
} lt_elf_osabi_t;

typedef
enum lt_elf_encoding {
	LT_ELFENC_LSB = 1,
	LT_ELFENC_MSB = 2,
} lt_elf_encoding_t;

typedef
enum lt_elf_objtype {
	LT_ELFTYPE_REL = 1,
	LT_ELFTYPE_EXEC = 2,
	LT_ELFTYPE_DYN = 3,
	LT_ELFTYPE_CORE = 4,
} lt_elf64_objtype_t;

typedef
enum lt_elf_arch {
	LT_ELFARCH_I386 = 3,
	LT_ELFARCH_AMD64 = 62,
} lt_elf_arch_t;

typedef
LT_PACKED_STRUCT(lt_elf64_fh) {
	u8 magic[4];

	u8 class;
	u8 encoding;
	u8 header_version;
	u8 osabi;
	u8 abi_version;

	u8 pad[7];

	u16 obj_type;
	u16 arch;
	u32 version;

	u64 entry;
	u64 ph_offset;
	u64 sh_offset;

	u32 cpu_flags;

	u16 fh_size;

	u16 ph_size;
	u16 ph_count;
	u16 sh_size;
	u16 sh_count;

	u16 sh_strtab_index;
} lt_elf64_fh_t;

typedef
enum lt_elf_ph_type {
	LT_ELF_PH_TYPE_NULL = 0,
	LT_ELF_PH_TYPE_LOAD = 1,
	LT_ELF_PH_TYPE_DYNAMIC = 2,
	LT_ELF_PH_TYPE_INTERP = 3,
	LT_ELF_PH_TYPE_NOTE = 4,
	LT_ELF_PH_TYPE_SHLIB = 5,
	LT_ELF_PH_TYPE_PHDR = 6,
	LT_ELF_PH_TYPE_TLS = 7,
} lt_elf_ph_type_t;

typedef
enum lt_elf_ph_flags {
	LT_ELF_PH_X = 1,
	LT_ELF_PH_W = 2,
	LT_ELF_PH_R = 4,
} lt_elf_ph_flags_t;

typedef
LT_PACKED_STRUCT(lt_elf64_ph) {
	u32 type;
	u32 flags;
	u64 offset;

	u64 vaddr;
	u64 paddr;
	u64 file_size;
	u64 mem_size;
	u64 alignment;
} lt_elf64_ph_t;

typedef
LT_PACKED_STRUCT(lt_elf64_sh) {
	u32 name_stab_offs;

	u32 type;
	u64 flags;

	u64 addr;
	u64 offset;
	u64 size;

	u32 link;
	u32 info;

	u64 addr_align;
	u64 ent_size;
} lt_elf64_sh_t;

#endif
