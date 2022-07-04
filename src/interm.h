#ifndef INTERM_H
#define INTERM_H 1

#include <lt/lt.h>

#define FOR_EACH_ICODE() \
	ICODE_OP(INVAL) \
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
	ICODE_OP(TOI8) \
	ICODE_OP(TOI16) \
	ICODE_OP(TOI32) \
	ICODE_OP(TOI64) \
	ICODE_OP(TOU8) \
	ICODE_OP(TOU16) \
	ICODE_OP(TOU32) \
	ICODE_OP(TOU64) \
	ICODE_OP(DEC) \
	ICODE_OP(INC) \
	ICODE_OP(NEG) \
	ICODE_OP(NOT) \
	ICODE_OP(JMP) \
	ICODE_OP(CALL) \
	\
	ICODE_OP(CSETG) \
	ICODE_OP(CSETGE) \
	ICODE_OP(CSETL) \
	ICODE_OP(CSETLE) \
	ICODE_OP(CSETA) \
	ICODE_OP(CSETAE) \
	ICODE_OP(CSETB) \
	ICODE_OP(CSETBE) \
	ICODE_OP(CSETE) \
	ICODE_OP(CSETNE) \
	ICODE_OP(CSETZ) \
	ICODE_OP(CSETNZ) \
	\
	ICODE_OP(CJMPG) \
	ICODE_OP(CJMPGE) \
	ICODE_OP(CJMPL) \
	ICODE_OP(CJMPLE) \
	ICODE_OP(CJMPA) \
	ICODE_OP(CJMPAE) \
	ICODE_OP(CJMPB) \
	ICODE_OP(CJMPBE) \
	ICODE_OP(CJMPE) \
	ICODE_OP(CJMPNE) \
	ICODE_OP(CJMPZ) \
	ICODE_OP(CJMPNZ) \
	\
	ICODE_OP(COPY) \
	ICODE_OP(MCOPY) \
	ICODE_OP(SRESV) \
	ICODE_OP(LOAD) \
	ICODE_OP(STOR) \
	ICODE_OP(INT) \
	ICODE_OP(FLOAT) \
	ICODE_OP(SEG) \
	ICODE_OP(GETLBL) \
	ICODE_OP(LBL) \
	\
	ICODE_OP(BEGCALL) \
	ICODE_OP(SETARG) \
	ICODE_OP(GETARG) \
	ICODE_OP(ENTER) \
	ICODE_OP(EXIT) \
	ICODE_OP(RET) \
	ICODE_OP(SYSCALL)

typedef
enum icode_type {
#define ICODE_OP(x) IR_##x,
	FOR_EACH_ICODE()
#undef ICODE_OP
} icode_type_t;

typedef
enum ival_stype {
	IVAL_REG = 0,
	IVAL_IMM = 1,
 	IVAL_SEG = 2,
	IVAL_COM = 3,

	IVAL_REF = 16,
} ival_stype_t;

typedef
struct ival {
	u32 stype;
	u32 child_count;
	union {
		u64 uint_val;
		i64 int_val;
		f64 float_val;
		u32 reg;
		struct ival* children;
	};
} ival_t;

typedef
enum icode_size {
	ISZ_8 = 1,
	ISZ_16 = 2,
	ISZ_32 = 4,
	ISZ_64 = 8,
} icode_size_t;

typedef
struct icode {
	u32 op;
	u32 dst;
	union {
		u32 regs[2];
		u64 uint_val;
		i64 int_val;
		f64 float_val;
	};
	u64 size;
} icode_t;

lstr_t icode_op_str(u16 op);
lstr_t icode_size_str(u64 size);

#define ICODE_INIT(op, size, dst, ...) { (op), (dst), __VA_ARGS__, (size) }
#define ICODE(op, size, dst, ...) ((icode_t)ICODE_INIT((op), (size), (dst), __VA_ARGS__))

#define ICODE0(op, size) ICODE((op), (size), 0, .regs = {0, 0})
#define ICODE1(op, size, v1) ICODE((op), (size), (v1), .regs = {0, 0})
#define ICODE2(op, size, v1, v2) ICODE((op), (size), (v1), .regs = {(v2), 0})
#define ICODE3(op, size, v1, v2, v3) ICODE((op), (size), (v1), .regs = {(v2), (v3)})

#define IVAL_INIT(stype, ...) { (stype), __VA_ARGS__ }
#define IVAL(stype, ...) ((ival_t)IVAL_INIT((stype), __VA_ARGS__))

#define IMMI(i) IVAL(IVAL_IMM, .uint_val = (i))
#define IMMF(f) IVAL(IVAL_IMM, .float_val = (f))
#define REG(r) IVAL(IVAL_REG, .reg = (r))
#define REF(r) IVAL(IVAL_REG | IVAL_REF, .reg = (r))
#define SEG(s) IVAL(IVAL_SEG, .uint_val = (s))

#endif
