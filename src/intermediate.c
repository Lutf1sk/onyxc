// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

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
        // OR if the array has not been allocated yet,
        // allocate space for 1024 instructions.
        if (func->instr_alloc_count)
            func->instr_alloc_count *= 2;
        else
            func->instr_alloc_count = 1024;

        usz sz = func->instr_alloc_count * sizeof(Instr);
        func->instrs = realloc(func->instrs, sz);
        assert(func->instrs);
    }

    func->instrs[func->instr_count++] = instr;
}

void print_instr(Instr instr) {
    printf("%-2i %-12s ",
           instr_sz_bit_count(instr.op.out_sz), instr_op_str(instr.op.op));
    switch (instr.op.op) {
    case IN_JMP:
        printf("R:%zu", instr.reg);
        break;

    case IN_CALL:
        printf("R:%zu", instr.reg);
        break;

    case IN_RET:
        break;

    case IN_LOAD:
        printf("R:%zu <- [R:%zu]", instr.reg, instr.regs[0]);
        break;

    case IN_STORE:
        printf("[R:%zu] <- R:%zu", instr.reg, instr.regs[0]);
        break;

    case IN_LOAD_LABEL: {
        LenStr name = instr.sym_hnd.tab->names[instr.sym_hnd.offs];
        printf("R:%zu <- G:%.*s", instr.reg, (int)name.len, name.str);
    }   break;

    case IN_LOAD_LIT:
        printf("R:%zu <- %lu", instr.reg, instr.lit_uint);
        break;
    case IN_LOAD_FUNC:
        printf("R:%zu <- F:%zu", instr.reg, instr.func_offs);
        break;

    case IN_STORE_ARG:
        printf("A:%zu <- R:%zu", instr.lit_uint, instr.reg);
        break;
    case IN_STORE_RETVAL:
        printf("R:%zu ->", instr.reg);
        break;
    case IN_LOAD_ARG:
        printf("R:%zu <- A:%zu", instr.reg, instr.lit_uint);
        break;

    case IN_LOAD_RETVAL:
        printf("R:%zu <-", instr.reg);
        break;

    case IN_ADD:
        printf("R:%zu <- R:%zu + R:%zu",
               instr.reg, instr.regs[0], instr.regs[1]);
        break;

    case IN_SUB:
        printf("R:%zu <- R:%zu - R:%zu",
               instr.reg, instr.regs[0], instr.regs[1]);
        break;

    case IN_MUL:
        printf("R:%zu <- R:%zu * R:%zu",
               instr.reg, instr.regs[0], instr.regs[1]);
        break;

    case IN_DIV:
        printf("R:%zu <- R:%zu / R:%zu",
               instr.reg, instr.regs[0], instr.regs[1]);
        break;

    case IN_MOV:
        printf("R:%zu <- R:%zu", instr.reg, instr.regs[0]);
        break;

    case IN_WIDEN:
        printf("R:%zu <- %i:R:%zu",
               instr.reg, instr_sz_bit_count(instr.op.arg_sz), instr.regs[0]);
        break;

    case IN_NARROW:
        printf("R:%zu <- %i:R:%zu",
               instr.reg, instr_sz_bit_count(instr.op.arg_sz), instr.regs[0]);
        break;
    }
}

void free_func(IntermediateFunc* func) {
    if (func->instrs)
        free(func->instrs);
}
