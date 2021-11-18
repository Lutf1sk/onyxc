#include <lt/io.h>

#include "amd64.h"
#include "interm.h"
#include "segment.h"

#define REG_A	0b0000
#define REG_C	0b0001
#define REG_D	0b0010
#define REG_B	0b0011
#define REG_SP	0b0100
#define REG_BP	0b0101
#define REG_SI	0b0110
#define REG_DI	0b0111

#define REG_8	0b1000
#define REG_9	0b1001
#define REG_10	0b1010
#define REG_11	0b1011
#define REG_12	0b1100
#define REG_13	0b1101
#define REG_14	0b1110
#define REG_15	0b1111

#define REG_REX 0b1000

static lstr_t reg_names[AMD64_REG_COUNT][4] = {
	{ CLSTR("al"),		CLSTR("ax"),	CLSTR("eax"),	CLSTR("rax") },
	{ CLSTR("cl"),		CLSTR("cx"),	CLSTR("ecx"),	CLSTR("rcx") },
	{ CLSTR("dl"),		CLSTR("dx"),	CLSTR("edx"),	CLSTR("rdx") },
	{ CLSTR("bl"),		CLSTR("bx"),	CLSTR("ebx"),	CLSTR("rbx") },
	{ NLSTR(),			CLSTR("sp"),	CLSTR("esp"),	CLSTR("rsp") },
	{ NLSTR(),			CLSTR("bp"),	CLSTR("ebp"),	CLSTR("rbp") },
	{ CLSTR("sil"),		CLSTR("si"),	CLSTR("esi"),	CLSTR("rsi") },
	{ CLSTR("dil"),		CLSTR("di"),	CLSTR("edi"),	CLSTR("rdi") },

	{ CLSTR("r8b"),		CLSTR("r8w"),	CLSTR("r8d"),	CLSTR("r8")  },
	{ CLSTR("r9b"),		CLSTR("r9w"),	CLSTR("r9d"),	CLSTR("r9")  },
	{ CLSTR("r10b"),	CLSTR("r10w"),	CLSTR("r10d"),	CLSTR("r10") },
	{ CLSTR("r11b"),	CLSTR("r11w"),	CLSTR("r11d"),	CLSTR("r11") },
	{ CLSTR("r12b"),	CLSTR("r12w"),	CLSTR("r12d"),	CLSTR("r12") },
	{ CLSTR("r13b"),	CLSTR("r13w"),	CLSTR("r13d"),	CLSTR("r13") },
	{ CLSTR("r14b"),	CLSTR("r14w"),	CLSTR("r14d"),	CLSTR("r14") },
	{ CLSTR("r15b"),	CLSTR("r15w"),	CLSTR("r15d"),	CLSTR("r15") },
};

#define VMOD_REG 0b00
#define VMOD_MRM 0b01
#define VMOD_IMM 0b10

#define VARG_8	0
#define VARG_16	1
#define VARG_32	2
#define VARG_64	3

#define VARG_OP_EXT 0b01

#define VARG(n, x) (x << ((n) * 2 + 2))
#define VARG_GET(x, n) (((x) >> ((n) * 2 + 2)) & 0b11)

typedef
struct amd64_var {
	u8 args;
	u8 sizes;
} amd64_var_t;

typedef
struct amd64_op {
	lstr_t str;

	// Variations
	amd64_var_t* vars;
	u8 (*var_ops)[4];
	u8 var_count;
} amd64_op_t;

#define X64_ADD 0
#define X64_SUB 1
#define X64_AND 2
#define X64_OR 3
#define X64_XOR 4
#define X64_SHL 5
#define X64_SHR 6
#define X64_SAL 7
#define X64_SAR 8

#define X64_NEG 9
#define X64_INC 10
#define X64_DEC 11

#define X64_MOV 12
#define X64_LEA 13
#define X64_MOVZX 14
#define X64_MOVSX 15

#define X64_JMP 16
#define X64_CALL 17
#define X64_RET 18

#define X64_SYSCALL 19

#define X64_IR_ENTER 20
#define X64_IR_LEAVE 21

#define X64_DIV 22
#define X64_IDIV 23
#define X64_CQO 24
#define X64_MUL 25
#define X64_IMUL 26

static amd64_op_t ops[] = {
	{ CLSTR("add"), (amd64_var_t[]) {
		{ 2 | VARG(0, VMOD_REG) | VARG(1, VMOD_MRM), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_REG), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_32) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0x01 },
		{ 0x03 },
		{ 0, 0x81 },
	}, 3 },

	{ CLSTR("sub"), (amd64_var_t[]) {
		{ 2 | VARG(0, VMOD_REG) | VARG(1, VMOD_MRM), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_REG), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_32) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0x29 },
		{ 0x2B },
		{ 5, 0x81 },
	}, 3 },

	{ CLSTR("and"), (amd64_var_t[]) {
		{ 2 | VARG(0, VMOD_REG) | VARG(1, VMOD_MRM), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_REG), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_32) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0x23 },
		{ 0x21 },
		{ 4, 0x81 },
	}, 3 },

	{ CLSTR("or"), (amd64_var_t[]) {
		{ 2 | VARG(0, VMOD_REG) | VARG(1, VMOD_MRM), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_REG), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_32) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0x0B },
		{ 0x09 },
		{ 1, 0x81 },
	}, 3 },

	{ CLSTR("xor"), (amd64_var_t[]) {
		{ 2 | VARG(0, VMOD_REG) | VARG(1, VMOD_MRM), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_REG), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_32) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0x33 },
		{ 0x31 },
		{ 6, 0x81 },
	}, 3 },

	{ CLSTR("shl"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_8) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 4, 0xD3 },
		{ 4, 0xC1 },
	}, 2 },

	{ CLSTR("shr"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_8) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 5, 0xD3 },
		{ 5, 0xC1 },
	}, 2 },

	{ CLSTR("sal"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_8) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 4, 0xD3 },
		{ 4, 0xC1 },
	}, 2 },

	{ CLSTR("sar"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_8) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 7, 0xD3 },
		{ 7, 0xC1 },
	}, 2 },

	{ CLSTR("neg"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 3, 0xF7 },
	}, 1 },

	{ CLSTR("inc"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0, 0xFF },
	}, 1 },

	{ CLSTR("dec"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 1, 0xFF },
	}, 1 },

	{ CLSTR("mov"), (amd64_var_t[]) {
		{ 2 | VARG(0, VMOD_REG) | VARG(1, VMOD_MRM), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_REG), VARG(0, VARG_64) | VARG(1, VARG_64) },
		{ 2 | VARG(0, VMOD_MRM) | VARG(1, VMOD_IMM), VARG(0, VARG_64) | VARG(1, VARG_32) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0x8B },
		{ 0x89 },
		{ 0, 0xC7 },
	}, 3 },

	{ CLSTR("lea"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_REG), VARG(0, VARG_64) },
	}, (u8[][4]){
		{ 0x8D },
	}, 1 },

	{ CLSTR("movzx") },
	{ CLSTR("movsx") },

	{ CLSTR("jmp"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) },
		{ 1 | VARG(0, VMOD_IMM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0xE9 },
		{ 4, 0xFF },
	}, 2 },

	{ CLSTR("call"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) },
		{ 1 | VARG(0, VMOD_IMM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 0xE8 },
		{ 2, 0xFF },
	}, 2 },

	{ CLSTR("ret"), (amd64_var_t[]) {
		{ 0, 0 },
	}, (u8[][4]){
		{ 0xCB },
	}, 1 },

	{ CLSTR("syscall"), (amd64_var_t[]) {
		{ 0, 0 },
	}, (u8[][4]){
		{ 0x0F, 0x05 },
	}, 1 },

	{ CLSTR("ir_enter"), (amd64_var_t[]) {
		{ 0, 0 },
	}, (u8[][4]){
		{ }
	}, 0 },

	{ CLSTR("ir_leave"), (amd64_var_t[]) {
		{ 0, 0 },
	}, (u8[][4]){
		{ },
	}, 0 },

	{ CLSTR("div"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 6, 0xF7 },
	}, 1 },

	{ CLSTR("idiv"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 7, 0xF7 },
	}, 1 },

	{ CLSTR("cqo"), (amd64_var_t[]) {
		{ 0, 0 },
	}, (u8[][4]){
		{ 0x99 },
	}, 1 },

	{ CLSTR("mul"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 4, 0xF7 },
	}, 1 },

	{ CLSTR("imul"), (amd64_var_t[]) {
		{ 1 | VARG(0, VMOD_MRM), VARG(0, VARG_64) | VARG_OP_EXT },
	}, (u8[][4]){
		{ 5, 0xF7 },
	}, 1 },
};

#define PFX_LOCK	0xF0
#define PFX_REPNZ	0xF2
#define PFX_REP		0xF3
#define PFX_OPSZ	0x66
#define PFX_ADDRSZ	0x67

// 0100WRXB
// W - Promotes to 64-bit
// R - Extends ModRM reg
// X - Extends SIB index
// B - Extends ModRM rm
#define REX(w, r, x, b) ((0b0100 << 4) | (w << 3) | (r << 2) | (x << 1) | b)
#define REX_W (1 << 3)
#define REX_R (1 << 2)
#define REX_X (1 << 1)
#define REX_B (1 << 0)

// MMRRRMMM
#define MODRM(mod, reg, rm) ((mod << 6) | (reg << 3) | rm)
#define MOD_DREG	0b00
#define MOD_DSP8	0b01
#define MOD_DSP32	0b10
#define MOD_REG		0b11

// SSIIIBBB
#define SIB(scale, index, base) ((scale << 6) | (index << 3) | base)
#define SIB_S1 0
#define SIB_S2 1
#define SIB_S4 2
#define SIB_S8 3

#define AMD64_BLOCK_SIZE 512
#define AMD64_BLOCK_MASK (AMD64_BLOCK_SIZE-1)

static
usz emit(amd64_ctx_t* cx, amd64_instr_t instr) {
	LT_ASSERT(cx->curr_func != -1);

	seg_ent_t* ent = &cx->cs[cx->curr_func];

	if (!(ent->size & AMD64_BLOCK_MASK))
		ent->data = realloc(ent->data, (ent->size + AMD64_BLOCK_SIZE) * sizeof(amd64_instr_t));

	((amd64_instr_t*)ent->data)[ent->size] = instr;
	return ent->size++;
}

static
b8 ival_eq(ival_t* v1, ival_t* v2) {
	if (v1->stype != v2->stype || v1->size != v2->size || v1->disp != v2->disp)
		return 0;

	switch (v1->stype & ~IVAL_REF) {
	case IVAL_REG: return v1->reg == v2->reg;
	case IVAL_IMM: return v1->uint_val == v2->uint_val;
	case IVAL_DSO: return v1->dso == v2->dso;
	case IVAL_CSO: return v1->cso == v2->cso;
	}
	return 0;
}

static
u8 reg_alloc(amd64_ctx_t* cx) {
	for (usz i = 0; i < AMD64_REG_COUNT; ++i) {
		if (!cx->reg_allocated[i]) {
			cx->reg_allocated[i] = 1;
			return i;
		}
	}
	LT_ASSERT_NOT_REACHED();
	return REG_A;
}

static
void reg_free(amd64_ctx_t* cx, u8 reg) {
	LT_ASSERT(reg < AMD64_REG_COUNT);
	LT_ASSERT(cx->reg_allocated[reg]);
	cx->reg_allocated[reg] = 0;
}


static
void zero_reg(amd64_ctx_t* cx, u8 reg) {
	amd64_instr_t instr;
	instr.op = X64_XOR;
	instr.var = 0;
	instr.reg_rm = reg | (reg << 4);
	instr.mod = MOD_REG;
	emit(cx, instr);
}

static
void gen_modrm(amd64_ctx_t* cx, ival_t* iv, u8* mod, u8* reg_rm) {
	u8 rm;

	switch (iv->stype) {
	case IVAL_REG: rm = iv->reg & 0b1111; *mod = MOD_REG; break;
	case IVAL_IMM: LT_ASSERT_NOT_REACHED(); break;
	case IVAL_CSO: LT_ASSERT_NOT_REACHED(); break;
	case IVAL_DSO: LT_ASSERT_NOT_REACHED(); break;

	case IVAL_REG | IVAL_REF: rm = iv->reg & 0b1111; *mod = MOD_DREG; break;
	case IVAL_IMM | IVAL_REF: LT_ASSERT_NOT_REACHED(); break;
	case IVAL_CSO | IVAL_REF: LT_ASSERT_NOT_REACHED(); break;
	case IVAL_DSO | IVAL_REF: LT_ASSERT_NOT_REACHED(); break;
	}

	*reg_rm |= rm << 4;
}

static
u8 find_var2(u8 op, u8 dst, u8 src) {
	u8 var_count = ops[op].var_count;
	amd64_var_t* var = ops[op].vars;

	for (usz i = 0; i < var_count; ++i) {
		u8 args = var[i].args;
		if ((args & 0b11) == 2 && VARG_GET(args, 0) == dst && VARG_GET(args, 1) == src)
			return i;
	}

	LT_ASSERT_NOT_REACHED();
	return -1;
}

static
void mov_to_reg(amd64_ctx_t* cx, u8 reg, ival_t* iv) {
	amd64_instr_t instr;
	instr.op = X64_MOV;
	instr.reg_rm = reg;
	instr.var = find_var2(X64_MOV, VMOD_REG, VMOD_MRM);

	gen_modrm(cx, iv, &instr.mod, &instr.reg_rm);
	emit(cx, instr);
}

static
void mov_from_reg(amd64_ctx_t* cx, ival_t* iv, u8 reg) {
	amd64_instr_t instr;
	instr.op = X64_MOV;
	instr.reg_rm = reg;
	instr.var = find_var2(X64_MOV, VMOD_MRM, VMOD_REG);

	gen_modrm(cx, iv, &instr.mod, &instr.reg_rm);
	emit(cx, instr);
}

static
void gen_op3(amd64_ctx_t* cx, u8 op, ival_t* a1, ival_t* a2, ival_t* a3) {
	amd64_instr_t i;
	i.mod = MOD_REG;
	i.op = op;
	i.var = find_var2(op, VMOD_REG, VMOD_MRM);
	i.reg_rm = REG_A;
// 	gen_modrm(cx, a3, &i.mod, &i.reg_rm);
// 
// 	mov_to_reg(cx, REG_A, a2);
	emit(cx, i);
// 	mov_from_reg(cx, a1, REG_A);
}

static
void gen_op2(amd64_ctx_t* cx, u8 op, ival_t* a1, ival_t* a2) {
	amd64_instr_t i;
	i.mod = MOD_REG;
	i.op = op;
	emit(cx, i);
}

static
void gen_op1(amd64_ctx_t* cx, u8 op, ival_t* a1) {
	amd64_instr_t i;
	i.mod = MOD_REG;
	i.op = op;
	emit(cx, i);
}

static
void gen_op0(amd64_ctx_t* cx, u8 op) {
	amd64_instr_t i;
	i.mod = MOD_REG;
	i.op = op;
	emit(cx, i);
}

static
void convert_icode(amd64_ctx_t* cx, icode_t* ir) {
	amd64_instr_t i;
	memset(&i, 0, sizeof(i));
	i.mod = MOD_REG;

	switch (ir->op) {
	case IR_ADD: gen_op3(cx, X64_ADD, &ir->arg1, &ir->arg2, &ir->arg3); break;
	case IR_SUB: gen_op3(cx, X64_SUB, &ir->arg1, &ir->arg2, &ir->arg3); break;

	case IR_AND: gen_op3(cx, X64_AND, &ir->arg1, &ir->arg2, &ir->arg3); break;
	case IR_OR: gen_op3(cx, X64_OR, &ir->arg1, &ir->arg2, &ir->arg3); break;
	case IR_XOR: gen_op3(cx, X64_XOR, &ir->arg1, &ir->arg2, &ir->arg3); break;


	case IR_USHL:break;
	case IR_USHR: case IR_ISHR: break;
	case IR_ISHL: break;

	case IR_IREM:
		mov_to_reg(cx, REG_A, &ir->arg2);
		gen_op0(cx, X64_CQO);
		gen_op1(cx, X64_IDIV, &ir->arg3);
		mov_from_reg(cx, &ir->arg1, REG_D);
		break;

	case IR_UREM:
		mov_to_reg(cx, REG_A, &ir->arg2);
		zero_reg(cx, REG_D);
		gen_op1(cx, X64_IDIV, &ir->arg3);
		mov_from_reg(cx, &ir->arg1, REG_A);
		break;

	case IR_IDIV:
		mov_to_reg(cx, REG_A, &ir->arg2);
		gen_op0(cx, X64_CQO);
		gen_op1(cx, X64_IDIV, &ir->arg3);
		mov_from_reg(cx, &ir->arg1, REG_A);
		break;

	case IR_UDIV:
		mov_to_reg(cx, REG_A, &ir->arg2);
		zero_reg(cx, REG_D);
		gen_op1(cx, X64_IDIV, &ir->arg3);
		mov_from_reg(cx, &ir->arg1, REG_A);
		break;

	case IR_IMUL:
		mov_to_reg(cx, REG_A, &ir->arg2);
		gen_op1(cx, X64_IMUL, &ir->arg3);
		mov_from_reg(cx, &ir->arg1, REG_A);
		break;

	case IR_UMUL:
		mov_to_reg(cx, REG_A, &ir->arg2);
		gen_op1(cx, X64_MUL, &ir->arg3);
		mov_from_reg(cx, &ir->arg1, REG_A);
		break;

	case IR_NEG: gen_op2(cx, X64_NEG, &ir->arg1, &ir->arg2); break;

	case IR_INC: gen_op1(cx, X64_INC, &ir->arg1); break;
	case IR_DEC: gen_op1(cx, X64_DEC, &ir->arg1); break;

	case IR_ENTER:
		i.op = X64_IR_ENTER;
		emit(cx, i);
		break;

	case IR_GETARG:
		break;

	case IR_SETARG:
		break;

	case IR_SYSCALL:
		i.op = X64_SYSCALL;
		emit(cx, i);
		break;

	case IR_SRESV:
		break;

	case IR_MOV:
		i.op = X64_MOV;
		emit(cx, i);
		break;

	case IR_LEA:
		i.op = X64_LEA;
		emit(cx, i);
		break;

	case IR_IEXT:
		i.op = X64_MOVZX;
		emit(cx, i);
		break;

	case IR_UEXT:
		i.op = X64_MOVSX;
		emit(cx, i);
		break;

	case IR_CSETG: case IR_CSETGE: case IR_CSETL: case IR_CSETLE:
	case IR_CSETA: case IR_CSETAE: case IR_CSETB: case IR_CSETBE:
	case IR_CSETE: case IR_CSETNE: case IR_CSETZ: case IR_CSETNZ:
		break;

	case IR_CJMPG: case IR_CJMPGE: case IR_CJMPL: case IR_CJMPLE:
	case IR_CJMPA: case IR_CJMPAE: case IR_CJMPB: case IR_CJMPBE:
	case IR_CJMPE: case IR_CJMPNE: case IR_CJMPZ: case IR_CJMPNZ:
		break;

	case IR_JMP: gen_op1(cx, X64_JMP, &ir->arg1); break;
	case IR_CALL: gen_op1(cx, X64_CALL, &ir->arg1); break;

	case IR_RET:
		i.op = X64_IR_LEAVE;
		emit(cx, i);
		i.op = X64_RET;
		emit(cx, i);
		break;

	default:
		lt_printf("%S\n", icode_type_str(ir->op));
		LT_ASSERT_NOT_REACHED();
	}
}

void amd64_gen(amd64_ctx_t* cx) {
	for (usz i = 0; i < cx->cs_count; ++i) {
		memset(cx->reg_allocated, 0, sizeof(cx->reg_allocated));
		cx->reg_allocated[REG_A] = 1;
		cx->reg_allocated[REG_C] = 1;
		cx->reg_allocated[REG_D] = 1;
		cx->reg_allocated[REG_SP] = 1;
		cx->reg_allocated[REG_BP] = 1;
		cx->reg_allocated[REG_12] = 1;
		cx->reg_allocated[REG_13] = 1;

		memset(cx->reg_map, 0, sizeof(cx->reg_map));

		lt_printf("Assembling cs'%uq\n", i);

		cx->curr_func = i;
		seg_ent_t* cs = &cx->ir_cs[i];
		for (usz j = 0; j < cs->size; ++j) {
			icode_t* ic = &((icode_t*)cs->data)[j];
			convert_icode(cx, ic);
		}
	}
}

static
void print_modrm(u8 mod, u8 rm, u8 size, u32 disp) {
	switch (mod) {
		// TODO: Handle SIB and RIP-relative addressing modes
		case MOD_DREG: lt_printf("[%S]", reg_names[rm][size]); break;
		case MOD_DSP8: lt_printf("[%S + %id]", reg_names[rm][size], disp); break;
		case MOD_DSP32: lt_printf("[%S + %id]", reg_names[rm][size], disp); break;
		case MOD_REG: lt_printf("%S", reg_names[rm][size]); break;
	}
}

void amd64_print_instr(amd64_instr_t instr) {
	amd64_op_t* op = &ops[instr.op];
	amd64_var_t* var = &op->vars[instr.var];

	usz arg_count = var->args & 0b11;

	lt_printf("%ud %S ", arg_count, op->str);

	u8 reg = instr.reg_rm & 0b1111;
	u8 rm = instr.reg_rm >> 4;
	u8 sizes = var->sizes, args = var->args;

	while (arg_count--) {
		u8 size = (sizes >>= 2) & 0b11;

		switch ((args >>= 2) & 0b11) {
		case VMOD_REG: lt_printf("%S", reg_names[reg][size]); break;
		case VMOD_MRM: print_modrm(instr.mod, rm, size, instr.disp); break;
		case VMOD_IMM: lt_printf("%iq", instr.imm); break;
		}

		if (arg_count)
			lt_printls(CLSTR(", "));
	}
}

