#ifndef INTERMEDIATE_H
#define INTERMEDIATE_H

#include "fwd.h"
#include "common.h"
#include "sym.h"

#define FOR_EACH_INSTR_OP() \
    INSTR_OP(MOV) \
    INSTR_OP(CALL) \
    INSTR_OP(JMP) \
    INSTR_OP(RET) \
    \
    INSTR_OP(LOAD) \
    INSTR_OP(STORE) \
    INSTR_OP(LOAD_FUNC) \
    INSTR_OP(LOAD_LABEL) \
    INSTR_OP(LOAD_LIT) \
    \
    INSTR_OP(ADD) \
    INSTR_OP(SUB) \
    INSTR_OP(MUL) \
    INSTR_OP(DIV)

typedef enum InstrSize {
	ISZ_8 = 0,
	ISZ_16 = 1,
	ISZ_32 = 2,
	ISZ_64 = 3,
} InstrSize;

int instr_sz_bit_count(InstrSize sz);

typedef enum InstrType {
	ITP_NONE = 0,
	ITP_FLOAT = 1,
	ITP_UINT = 2,
	ITP_SINT = 3,
} InstrType;

typedef
enum InstrOpNum {
#define INSTR_OP(x) IN_##x,
	FOR_EACH_INSTR_OP()
#undef INSTR_OP
} InstrOpNum;

typedef
struct PACKED InstrOp {
    InstrOpNum op : 12;
	u8 size : 2;
	u8 type : 2;
} InstrOp;

typedef
struct Instr {
	InstrOp op;
    u64 reg;
	union {
        u64 lit_uint;
        i64 lit_sint;
        f64 lit_float;

        u64 regs[2];
        SymbolHandle sym_hnd;
        usz func_offs;
	};
} Instr;

typedef
struct IntermediateFunc {
	usz instr_count;
	usz instr_alloc_count;
	Instr* instrs;
} IntermediateFunc;

const char* instr_op_str(InstrOpNum op);

void add_instr(IntermediateFunc* func, Instr instr);

void free_func(IntermediateFunc* func);

static inline INLINE
IntermediateFunc make_intermediate_func() {
	return (IntermediateFunc) { 0, 0, NULL };
}

static inline INLINE
InstrOp make_instr_op(InstrOpNum num, InstrSize size, InstrType type) {
	return (InstrOp) { num, size, type };
}

static inline INLINE
Instr make_instr_r(InstrOp op, u64 reg, u64 arg2, u64 arg3) {
    return (Instr) { op, reg, .regs = { arg2, arg3 } };
}

static inline INLINE
Instr make_instr(InstrOp op, u64 reg) {
    return (Instr) { op, reg };
}

#endif // INTERMEDIATE_H
