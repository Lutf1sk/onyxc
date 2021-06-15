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
	INSTR_OP(LOAD_ADDR) \
    INSTR_OP(STORE_ADDR) \
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
	u16 op : 12;
	u8 size : 2;
	u8 type : 2;
} InstrOp;

typedef
struct Instr {
	InstrOp op;
	union {
		u64 uargs[3];
		i64 iargs[3];
		f64 fargs[3];
		struct {
			u64 sym_reg;
			SymbolHandle sym_hnd;
		};
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
Instr make_instr_u(InstrOp op, u64 arg1, u64 arg2, u64 arg3) {
	return (Instr) { op, .uargs = { arg1, arg2, arg3 } };
}

static inline INLINE
Instr make_instr_i(InstrOp op, i64 arg1, i64 arg2, i64 arg3) {
	return (Instr) { op, .iargs = { arg1, arg2, arg3 } };
}

static inline INLINE
Instr make_instr_f(InstrOp op, f64 arg1, f64 arg2, f64 arg3) {
	return (Instr) { op, .fargs = { arg1, arg2, arg3 } };
}

#endif // INTERMEDIATE_H
