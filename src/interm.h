#ifndef INTERM_H
#define INTERM_H 1

#include <lt/lt.h>

#define FOR_EACH_ICODE() \
	ICODE_OP(INVAL) \
	ICODE_OP(ADD) \
	ICODE_OP(SUB) \
	ICODE_OP(MUL) \
	ICODE_OP(IDIV) \
	ICODE_OP(UDIV) \
	ICODE_OP(IREM) \
	ICODE_OP(UREM) \
	ICODE_OP(AND) \
	ICODE_OP(OR) \
	ICODE_OP(XOR) \
	ICODE_OP(SHL) \
	ICODE_OP(SHR) \
	ICODE_OP(SAR) \
	ICODE_OP(SAL) \
	ICODE_OP(IEXT) \
	ICODE_OP(UEXT) \
	ICODE_OP(NEG) \
	ICODE_OP(NOT) \
	ICODE_OP(LOAD) \
	ICODE_OP(STORE) \
	ICODE_OP(JMP) \
	ICODE_OP(ARG) \
	ICODE_OP(CALL) \
	ICODE_OP(RET) \
	ICODE_OP(COPY) \
	ICODE_OP(CSET) \
	ICODE_OP(CMOV) \
	ICODE_OP(CJMP) \

typedef
enum icode_type {
#define ICODE_OP(x) IR_##x,
	FOR_EACH_ICODE()
#undef ICODE_OP
} icode_type_t;

typedef
enum icode_size {
	ISZ_8 = 0,
	ISZ_16 = 1,
	ISZ_32 = 2,
	ISZ_64 = 3,
} icode_size_t;

typedef
struct icode {
	u8 op;
	u8 sz1, sz2;
	u64 arg1, arg2;
} icode_t;

lstr_t icode_type_str(icode_type_t type);
lstr_t icode_size_str(icode_size_t size);

#endif
