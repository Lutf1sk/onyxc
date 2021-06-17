#include <stdlib.h>
#include <assert.h>

#include "intermediate.h"

int instr_sz_bit_count(InstrSize sz) {
    switch (sz) {
    case ISZ_8: return 8;
    case ISZ_16: return 16;
    case ISZ_32: return 32;
    case ISZ_64: return 64;
    }
}

const char* instr_op_str(InstrOpNum op) {
    switch (op) {
#define INSTR_OP(x) case IN_##x: return #x;
        FOR_EACH_INSTR_OP();
#undef INSTR_OP
        default:
            return NULL;
    }
}

void add_instr(IntermediateFunc* func, Instr instr) {
    if (func->instr_count >= func->instr_alloc_count) {
        // Double the size and reallocate the array
        // OR if the array has not been allocated yet, allocate space for 1024 instructions.
        if (func->instr_alloc_count)
            func->instr_alloc_count *= 2;
        else
            func->instr_alloc_count = 1024;

        func->instrs = realloc(func->instrs, func->instr_alloc_count * sizeof(Instr));
        assert(func->instrs);
    }

    func->instrs[func->instr_count++] = instr;
}

void free_func(IntermediateFunc* func) {
    if (func->instrs)
        free(func->instrs);
}
