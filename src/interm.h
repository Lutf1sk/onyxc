#ifndef INTERM_H
#define INTERM_H 1

#include <lt/lt.h>

#define FOR_EACH_ICODE() \
	ICODE_OP(INVAL) \
	ICODE_OP(LEA) \
	ICODE_OP(MOV) \
	ICODE_OP(ADD) \
	ICODE_OP(SUB) \
	ICODE_OP(IMUL) \
	ICODE_OP(UMUL) \
	ICODE_OP(IDIV) \
	ICODE_OP(UDIV) \
	ICODE_OP(IREM) \
	ICODE_OP(UREM) \
	ICODE_OP(AND) \
	ICODE_OP(OR) \
	ICODE_OP(XOR) \
	ICODE_OP(ISHL) \
	ICODE_OP(USHL) \
	ICODE_OP(ISHR) \
	ICODE_OP(USHR) \
	ICODE_OP(IEXT) \
	ICODE_OP(UEXT) \
	ICODE_OP(DEC) \
	ICODE_OP(INC) \
	ICODE_OP(DECSFX) \
	ICODE_OP(INCSFX) \
	ICODE_OP(NEG) \
	ICODE_OP(NOT) \
	ICODE_OP(JMP) \
	ICODE_OP(SETARG) \
	ICODE_OP(GETARG) \
	ICODE_OP(CALL) \
	ICODE_OP(RET) \
	ICODE_OP(RETVAL) \
	ICODE_OP(COPY) \
	\
	ICODE_OP(CMOVE) \
	ICODE_OP(CMOVG) \
	ICODE_OP(CMOVGE) \
	ICODE_OP(CMOVL) \
	ICODE_OP(CMOVLE) \
	ICODE_OP(CMOVNE) \
	ICODE_OP(CMOVZ) \
	ICODE_OP(CMOVNZ) \
	\
	ICODE_OP(CJMPE) \
	ICODE_OP(CJMPG) \
	ICODE_OP(CJMPGE) \
	ICODE_OP(CJMPL) \
	ICODE_OP(CJMPLE) \
	ICODE_OP(CJMPNE) \
	ICODE_OP(CJMPZ) \
	ICODE_OP(CJMPNZ) \
	\
	ICODE_OP(SRESV) \
	ICODE_OP(PUSH) \
	ICODE_OP(POP)

typedef
enum icode_type {
#define ICODE_OP(x) IR_##x,
	FOR_EACH_ICODE()
#undef ICODE_OP
} icode_type_t;

typedef
enum ival_size {
	ISZ_8 = 1,
	ISZ_16 = 2,
	ISZ_32 = 4,
	ISZ_64 = 8,
} icode_size_t;

typedef
enum ival_stype {
	IVAL_INVAL = 0,
	IVAL_IMM = 1,
	IVAL_REG = 2,
	IVAL_DSO = 3,
	IVAL_CSO = 4,
	IVAL_SFO = 5,

	IVAL_REF = 8,
} ival_stype_t;

typedef
struct ival {
	u8 size;
	u8 stype;
	union {
		usz reg;
		struct { u32 cso, instr; };
		u32 dso;
		u32 sfo;
		u64 uint_val;
		i64 int_val;
		f64 float_val;
	};
} ival_t;

typedef
struct icode {
	u8 op;
	ival_t arg1, arg2, arg3;
} icode_t;

lstr_t icode_type_str(icode_type_t type);
lstr_t icode_size_str(icode_size_t size);

#define ICODE_INIT(op, v1, v2, v3) { (op), (v1), (v2), (v3) }
#define ICODE(op, v1, v2, v3) ((icode_t)ICODE_INIT(op, v1, v2, v3))

#define IVAL_INIT(size, flags, ...) {(size), (flags), __VA_ARGS__}
#define IVAL(size, flags, ...) ((ival_t)IVAL_INIT(size, flags, __VA_ARGS__))

#endif
