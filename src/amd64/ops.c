#include "ops.h"

amd64_op_t ops[] = {
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

