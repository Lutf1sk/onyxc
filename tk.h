#ifndef TK_H
#define TK_H

#include "common.h"

#define FOR_EACH_TOKEN() \
	TOKEN_OP(INVALID) \
	TOKEN_OP(PLUS) \
	TOKEN_OP(MINUS) \
	TOKEN_OP(ASTERISK) \
	TOKEN_OP(SLASH) \
	TOKEN_OP(IDENTIFIER) \
	TOKEN_OP(INTEGER) \
	TOKEN_OP(FLOAT) \
	TOKEN_OP(STRING) \
	TOKEN_OP(CHARACTER) \
	TOKEN_OP(COLON) \
	TOKEN_OP(DOUBLE_COLON) \
	TOKEN_OP(SEMICOLON) \
	TOKEN_OP(COMMA) \
	TOKEN_OP(DOT) \
	TOKEN_OP(LEFT_PARENTH) \
	TOKEN_OP(RIGHT_PARENTH) \
	TOKEN_OP(LEFT_BRACE) \
	TOKEN_OP(RIGHT_BRACE) \
	TOKEN_OP(LEFT_BRACKET) \
	TOKEN_OP(RIGHT_BRACKET) \
	TOKEN_OP(KW_RETURN) \
	TOKEN_OP(KW_LET) \
	TOKEN_OP(KW_STRUCT)

typedef
enum TokenType {
#define TOKEN_OP(x) TK_##x,
	FOR_EACH_TOKEN()
#undef TOKEN_OP
} TokenType;

typedef
struct Token {
	TokenType type;
	usz start;
	usz len;
	usz line_index;
} Token;

const char* tk_type_str(TokenType type);

#endif // TK_H
