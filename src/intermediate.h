#ifndef INTERMEDIATE_H
#define INTERMEDIATE_H

#include "common.h"

#define FOR_EACH_INSTR_OP() \
	INSTR_OP(MOV) \
	INSTR_OP(ADD) \
	INSTR_OP(SUB) \
	INSTR_OP(MUL) \
	INSTR_OP(DIV) \
	INSTR_OP(JMP) \
	INSTR_OP(CALL) \
	INSTR_OP(RET)

typedef
enum InstrOp {
#define INSTR_OP(x) IN_##x,
	FOR_EACH_INSTR_OP()
#undef INSTR_OP
} InstrOp;

typedef
struct Instr {
	InstrOp op;
	u8 arg_count;
	union {
		u64 uargs[3];
		i64 iargs[3];
		f64 fargs[3];
	}
} Instr;

typedef
struct IntermediateFunc {
	usz instr_count;
	usz instr_alloc_count;
	Instr* instrs;
} IntermediateFunc;

const char* instr_op_str(InstrOp op);

void add_instr(IntermediateFunc* func, Instr instr);

void free_func(IntermediateFunc* func);

static inline INLINE
IntermediateFunc make_intermediate_func() {
	return (IntermediateFunc) { 0, 0, NULL };
}

#define INSTR_3A(op, a1, a2, a3) ((Instr) { (op), 3, { (a1), (a2), (a3) } })
#define INSTR_2A(op, a1, a2) ((Instr) { (op), 2, { (a1), (a2), 0 } })
#define INSTR_1A(op, a1) ((Instr) { (op), 1, { (a1), 0, 0 } })
#define INSTR_0A(op) ((Instr) { (op), 0, { 0, 0, 0 } })

#endif // INTERMEDIATE_H
