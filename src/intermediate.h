#ifndef INTERMEDIATE_H
#define INTERMEDIATE_H

#include "common.h"

#define FOR_EACH_INSTR_OP() \
	INSTR_OP(MOV) \
	INSTR_OP(ADD) \
	INSTR_OP(SUB) \
	INSTR_OP(MUL) \
	INSTR_OP(DIV) \
	INSTR_OP(JMP)

typedef
enum InstrOp {
#define INSTR_OP(x) IN_##x,
	FOR_EACH_INSTR_OP()
#undef INSTR_OP
} InstrOp;

typedef
struct Instr {
	InstrOp op;
} Instr;

typedef
struct IntermediateFunc {
	LenStr export_name;
	usz instr_count;
	usz instr_alloc_count;
	Instr* instrs;
} IntermediateFunc;

const char* instr_op_str(InstrOp op);

void add_instr(IntermediateFunc* func, Instr instr);

void free_func(IntermediateFunc* func);

static inline INLINE
IntermediateFunc make_func(const LenStr name) {
	return (IntermediateFunc) { name, 0, 0, NULL };
}

#endif // INTERMEDIATE_H
