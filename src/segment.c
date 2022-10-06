#include <lt/io.h>

#include "segment.h"
#include "interm.h"
#include "textattrib.h"

#define SEGMENT_BLOCK_SIZE 16
#define SEGMENT_BLOCK_MASK (SEGMENT_BLOCK_SIZE-1)

usz add_segment(segtab_t* tab, seg_ent_t* seg) {
	if (!(tab->count & SEGMENT_BLOCK_MASK))
		tab->seg = realloc(tab->seg, (tab->count + SEGMENT_BLOCK_SIZE) * sizeof(seg_ent_t));
	tab->seg[tab->count] = *seg;
	return tab->count++;
}

void print_segment(segtab_t* tab, usz i) {
	seg_ent_t* seg = &tab->seg[i];
	if (seg->stype == SEG_DATA) {
		lt_printf("DS %uz '%S': %uq bytes\n", i, seg->name, seg->size);
		return;
	}

	icode_t* icode = seg->icode_data;
	lt_printf("CS %uz '%S':\n", i, seg->name);
	for (usz i = 0; i < seg->icode_count; ++i) {
		icode_t ic = icode[i];

		lt_printf("%uz\t%S\t%S\t", i, icode_size_str(ic.size), icode_op_str(ic.op));

		if (ic.dst)
			lt_printf(A_BOLD"r%iq "A_RESET, ic.dst);
		switch (ic.op) {
		case IR_INT: lt_printf("0x%hq\n", ic.uint_val); break;
		case IR_FLOAT: lt_printf("FLOAT\n"); break;
		case IR_SRESV: lt_printf("%ud %ud\n", ic.regs[0], ic.regs[1]); break;
		case IR_GETLBL: lt_printf("%iq\n", ic.int_val); break;
		case IR_LBL: lt_printf("%ud\n", ic.uint_val); break;
		case IR_SEG: lt_printf("%ud\n", ic.regs[0]); break;
		case IR_CALL: lt_printf("r%ud %ud\n", ic.regs[0], ic.regs[1]); break;
		case IR_SYSCALL: lt_printf("%ud\n", ic.regs[0]); break;
		case IR_SETARG: lt_printf("%ud\n", ic.regs[0]); break;
		case IR_GETARG: lt_printf("%ud\n", ic.regs[0]); break;
		default:
			for (usz i = 0; i < 2; ++i)
				if (ic.regs[i])
					lt_printf("r%ud ", ic.regs[i]);
			lt_printf("\n");
			break;
		}
	}
}

