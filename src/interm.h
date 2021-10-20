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
	ICODE_OP(CSETE) \
	ICODE_OP(CSETG) \
	ICODE_OP(CSETGE) \
	ICODE_OP(CSETL) \
	ICODE_OP(CSETLE) \
	ICODE_OP(CSETA) \
	ICODE_OP(CSETAE) \
	ICODE_OP(CSETB) \
	ICODE_OP(CSETBE) \
	ICODE_OP(CSETNE) \
	ICODE_OP(CSETZ) \
	ICODE_OP(CSETNZ) \
	\
	ICODE_OP(CJMPE) \
	ICODE_OP(CJMPG) \
	ICODE_OP(CJMPGE) \
	ICODE_OP(CJMPL) \
	ICODE_OP(CJMPLE) \
	ICODE_OP(CJMPA) \
	ICODE_OP(CJMPAE) \
	ICODE_OP(CJMPB) \
	ICODE_OP(CJMPBE) \
	ICODE_OP(CJMPNE) \
	ICODE_OP(CJMPZ) \
	ICODE_OP(CJMPNZ) \
	\
	ICODE_OP(SRESV) \
	ICODE_OP(PUSH) \
	ICODE_OP(POP) \
	\
	ICODE_OP(ENTER) \
	ICODE_OP(EXIT) \
	ICODE_OP(SYSCALL)

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
	u16 size;
	u8 stype;
	u8 scale;
	u32 index;
	union {
		usz reg;
		usz cso;
		usz dso;
		usz sfo;
		u64 uint_val;
		i64 int_val;
		f64 float_val;
	};
} ival_t;

typedef
struct icode {
	ival_t arg1, arg2, arg3;
	u8 op;
} icode_t;

lstr_t icode_type_str(icode_type_t type);
lstr_t icode_size_str(icode_size_t size);

#define ICODE_INIT(op, v1, v2, v3) { (v1), (v2), (v3), (op) }
#define ICODE(op, v1, v2, v3) ((icode_t)ICODE_INIT(op, v1, v2, v3))

#define ICODE0(op) ICODE(op, IVAL(0, 0), IVAL(0, 0), IVAL(0, 0))
#define ICODE1(op, v1) ICODE(op, v1, IVAL(0, 0), IVAL(0, 0))
#define ICODE2(op, v1, v2) ICODE(op, v1, v2, IVAL(0, 0))
#define ICODE3(op, v1, v2, v3) ICODE(op, v1, v2, v3)

#define IVAL_INIT(size, flags, ...) {(size), (flags), 0, 0, __VA_ARGS__}
#define IVAL(size, flags, ...) ((ival_t)IVAL_INIT(size, flags, __VA_ARGS__))

#endif
