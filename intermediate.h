#ifndef INTERMEDIATE_H
#define INTERMEDIATE_H

#define FOR_EACH_INSTR() \
	INSTR_OP(MOV) \
	INSTR_OP(ADD) \
	INSTR_OP(SUB) \
	INSTR_OP(MUL) \
	INSTR_OP(DIV) \
	INSTR_OP(JMP)

typedef
enum InstrOp {
#define INSTR_OP(x) IN_##x,
	FOR_EACH_INSTR()
#undef INSTR_OP
} InstrOp;

typedef
struct Instr {
	InstrOp op;
} Instr;

#endif // INTERMEDIATE_H
