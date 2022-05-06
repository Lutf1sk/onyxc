#ifndef TK_H
#define TK_H 1

#include "fwd.h"

#include <lt/lt.h>

#define FOR_EACH_TK() \
	TK_OP(INVALID) \
	TK_OP(EOF) \
	TK_OP(IDENTIFIER) \
	TK_OP(NUMBER) \
	TK_OP(CHAR) \
	TK_OP(STRING) \
	\
	TK_OP(LEFT_BRACKET) \
	TK_OP(RIGHT_BRACKET) \
	TK_OP(LEFT_BRACE) \
	TK_OP(RIGHT_BRACE) \
	TK_OP(LEFT_PARENTH) \
	TK_OP(RIGHT_PARENTH) \
	\
	TK_OP(COLON) \
	TK_OP(DOUBLE_COLON) \
	TK_OP(SEMICOLON) \
	TK_OP(DOT) \
	TK_OP(DOUBLE_DOT) \
	TK_OP(COMMA) \
	\
	TK_OP(PLUS) \
	TK_OP(DOUBLE_PLUS) \
	TK_OP(MINUS) \
	TK_OP(DOUBLE_MINUS) \
	TK_OP(EXCLAMATION) \
	TK_OP(TILDE) \
	TK_OP(ASTERISK) \
	TK_OP(AMPERSAND) \
	TK_OP(DOUBLE_AMPERSAND) \
	TK_OP(SLASH) \
	TK_OP(PERCENT) \
	TK_OP(LESSER) \
	TK_OP(DOUBLE_LESSER) \
	TK_OP(GREATER) \
	TK_OP(DOUBLE_GREATER) \
	TK_OP(LESSER_EQUAL) \
	TK_OP(GREATER_EQUAL) \
	TK_OP(EQUAL) \
	TK_OP(DOUBLE_EQUAL) \
	TK_OP(EXCLAMATION_EQUAL) \
	TK_OP(CARET) \
	TK_OP(PIPE) \
	TK_OP(DOUBLE_PIPE) \
	TK_OP(PLUS_EQUAL) \
	TK_OP(MINUS_EQUAL) \
	TK_OP(ASTERISK_EQUAL) \
	TK_OP(SLASH_EQUAL) \
	TK_OP(PERCENT_EQUAL) \
	TK_OP(DOUBLE_LESSER_EQUAL) \
	TK_OP(DOUBLE_GREATER_EQUAL) \
	TK_OP(AMPERSAND_EQUAL) \
	TK_OP(CARET_EQUAL) \
	TK_OP(PIPE_EQUAL) \
	\
	TK_OP(MINUS_GREATER) \
	\
	TK_OP(KW_BREAK) \
	TK_OP(KW_CONTINUE) \
	TK_OP(KW_CASE) \
	TK_OP(KW_EXPLICIT) \
	TK_OP(KW_ELSE) \
	TK_OP(KW_ELIF) \
	TK_OP(KW_FOR) \
	TK_OP(KW_IF) \
	TK_OP(KW_IMPLICIT) \
	TK_OP(KW_LET) \
	TK_OP(KW_DEF) \
	TK_OP(KW_RETURN) \
	TK_OP(KW_SWITCH) \
	TK_OP(KW_STRUCT) \
	TK_OP(KW_WHILE) \
	TK_OP(KW_NULL) \
	TK_OP(KW_SYSCALL) \
	TK_OP(KW_IMPORT) \
	\
	TK_OP(WHITESPACE) \
	TK_OP(MAX)

typedef enum tk_stype {
#define TK_OP(x) TK_##x,
	FOR_EACH_TK()
#undef TK_OP
} tk_stype_t;

typedef struct tk {
	u32 stype;
	u32 line_index;
	lstr_t str;
	lex_ctx_t* cx;
} tk_t;

lstr_t tk_type_str(tk_stype_t stype);

usz unescape_str(char* out, tk_t* tk);

#define TK_INIT(cx, stype, str, line) { (stype), (line), (str), (cx) }
#define TK(cx, stype, str, line) ((tk_t)TK_INIT(cx, stype, str, line))

#endif
