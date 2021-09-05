#include "lex.h"

#include <lt/str.h>
#include <lt/mem.h>
#include <lt/io.h>

#include <ctype.h>

static LT_INLINE
b8 is_identifier_head(int c) {
	return isalpha(c) || c == '_';
}

static LT_INLINE
b8 is_identifier_body(int c) {
	return isalnum(c) || c == '_';
}

static
tk_stype_t identifier_type(lstr_t str) {
	if (str.len < 2)
		return TK_IDENTIFIER;

	switch (*str.str) {
	case 'b':
		if (lt_lstr_eq(str, CLSTR("break"))) return TK_KW_BREAK;
		break;

	case 'c':
		if (lt_lstr_eq(str, CLSTR("continue"))) return TK_KW_CONTINUE;
		if (lt_lstr_eq(str, CLSTR("case"))) return TK_KW_CASE;
		break;

	case 'd':
		if (lt_lstr_eq(str, CLSTR("def"))) return TK_KW_DEF;
		break;

	case 'e':
		if (lt_lstr_eq(str, CLSTR("explicit"))) return TK_KW_EXPLICIT;
		if (lt_lstr_eq(str, CLSTR("else"))) return TK_KW_ELSE;
		if (lt_lstr_eq(str, CLSTR("elif"))) return TK_KW_ELIF;
		break;

	case 'f':
		if (lt_lstr_eq(str, CLSTR("for"))) return TK_KW_FOR;
		break;

	case 'i':
		if (lt_lstr_eq(str, CLSTR("implicit"))) return TK_KW_IMPLICIT;
		if (lt_lstr_eq(str, CLSTR("if"))) return TK_KW_IF;
		break;

	case 'l':
		if (lt_lstr_eq(str, CLSTR("let"))) return TK_KW_LET;
		break;

	case 'r':
		if (lt_lstr_eq(str, CLSTR("return"))) return TK_KW_RETURN;
		break;

	case 's':
		if (lt_lstr_eq(str, CLSTR("switch"))) return TK_KW_SWITCH;
		if (lt_lstr_eq(str, CLSTR("struct"))) return TK_KW_STRUCT;
		break;

	case 'w':
		if (lt_lstr_eq(str, CLSTR("while"))) return TK_KW_WHILE;
		break;

	default:
		break;
	}

	return TK_IDENTIFIER;
}

static u8 chars[256];

#define emit(x) (out_tk[tk_count++] = (tk_t){ (x), LSTR(&cx->data.str[tk_start], it - tk_start), line_index })

static
usz lex_cached(lex_ctx_t* cx, tk_t* out_tk) {
	usz tk_count = 0;
	char* data = cx->data.str;

	usz it = 0;
	usz line_index = 0;

	for (;;) {
		char c = data[it];
		if (!c)
			return tk_count;

		usz tk_start = it++;
		u8 tk = chars[(u8)c];

		switch (tk) {
		case TK_COLON: c = data[it];
			if (c == ':') { ++it; emit(TK_DOUBLE_COLON); }
			else emit(TK_COLON);
			break;

		case TK_PLUS: c = data[it];
			if (c == '+') { ++it; emit(TK_DOUBLE_PLUS); }
			else if (c == '=') { ++it; emit(TK_PLUS_EQUAL); }
			else emit(TK_PLUS);
			break;

		case TK_MINUS: c = data[it];
			if (c == '-') { ++it; emit(TK_DOUBLE_MINUS); }
			else if (c == '=') { ++it; emit(TK_MINUS_EQUAL); }
			else if (c == '>') { ++it; emit(TK_MINUS_GREATER); }
			else emit(TK_MINUS);
			break;

		case TK_EXCLAMATION: c = data[it];
			if (c == '=') { ++it; emit(TK_EXCLAMATION_EQUAL); }
			else emit(TK_EXCLAMATION);
			break;

		case TK_TILDE: emit(TK_TILDE); break;

		case TK_ASTERISK: c = data[it];;
			if (c == '=') { ++it; emit(TK_ASTERISK_EQUAL); }
			else emit(TK_ASTERISK);
			break;

		case TK_AMPERSAND: c = data[it];
			if (c == '&') { ++it; emit(TK_DOUBLE_AMPERSAND); }
			else if (c == '=') { ++it; emit(TK_AMPERSAND_EQUAL); }
			else emit(TK_AMPERSAND);
			break;

		case TK_SLASH: c = data[it];
			if (c == '=') { ++it; emit(TK_SLASH_EQUAL); }
			else emit(TK_SLASH);
			break;

		case TK_PERCENT: c = data[it];
			if (c == '=') { ++it; emit(TK_PERCENT_EQUAL); }
			else emit(TK_PERCENT);
			break;

		case TK_LESSER: c = data[it];
			if (c == '=') { ++it; emit(TK_LESSER_EQUAL); }
			else if (c == '<') { c = data[++it];
				if (c == '=') { ++it; emit(TK_DOUBLE_LESSER_EQUAL); }
				else emit(TK_DOUBLE_LESSER);
			}
			else emit(TK_LESSER);
			break;

		case TK_GREATER: c = data[it];
			if (c == '=') { ++it; emit(TK_GREATER_EQUAL); }
			else if (c == '>') { c = data[++it];
				if (c == '=') { ++it; emit(TK_DOUBLE_GREATER_EQUAL); }
				else emit(TK_DOUBLE_GREATER);
			}
			else emit(TK_GREATER);
			break;

		case TK_CARET: c = data[it];
			if (c == '=') { ++it; emit(TK_CARET_EQUAL); }
			else emit(TK_CARET);
			break;

		case TK_PIPE: c = data[it];
			if (c == '=') { ++it; emit(TK_PIPE_EQUAL); }
			else if (c == '|') { ++it; emit(TK_DOUBLE_PIPE); }
			else emit(TK_PIPE);
			break;

		case TK_EQUAL: c = data[it];
			if (c == '=') { ++it; emit(TK_DOUBLE_EQUAL); }
			else emit(TK_EQUAL);
			break;

		case TK_INT: {
			while (isdigit(data[it]))
				++it;
			emit(TK_INT);
		}	break;

		case TK_IDENTIFIER: {
			while(is_identifier_body(data[it]))
				++it;

			tk_stype_t stype = identifier_type(LSTR(&data[tk_start], it - tk_start));
			emit(stype);
		}	break;

		case TK_WHITESPACE: {
			for (;;) {
				if (c == '\n')
					++line_index;
				c = data[it];
				if (chars[(u8)c] != TK_WHITESPACE)
					break;
				++it;
			}
		}	break;

		case '\0':
			lt_ferrf("%s:%uz: Unexpected character '%c'\n", cx->path, line_index + 1, c);

		default:
			emit(tk);
			break;
		}
	}
}

usz lex_initial(lex_ctx_t* cx, tk_t* out_tk) {
	memset(chars, 0, sizeof(chars));

	// Fill characters
	for (usz i = 'A'; i <= 'Z'; ++i)
		chars[i] = TK_IDENTIFIER;
	for (usz i = 'a'; i <= 'z'; ++i)
		chars[i] = TK_IDENTIFIER;
	chars['_'] = TK_IDENTIFIER;

	// Fill digits
	for (usz i = '0'; i <= '9'; ++i)
		chars[i] = TK_INT;

	// Fill whitespace
	chars[' '] = TK_WHITESPACE;
	chars['\t'] = TK_WHITESPACE;
	chars['\r'] = TK_WHITESPACE;
	chars['\n'] = TK_WHITESPACE;

	// Fill punctuation
	chars['.'] = TK_DOT;
	chars[','] = TK_COMMA;
	chars[':'] = TK_COLON;
	chars[';'] = TK_SEMICOLON;
	chars['('] = TK_LEFT_PARENTH;
	chars[')'] = TK_RIGHT_PARENTH;
	chars['{'] = TK_LEFT_BRACE;
	chars['}'] = TK_RIGHT_BRACE;
	chars['['] = TK_LEFT_BRACKET;
	chars[']'] = TK_RIGHT_BRACKET;

	// Fill operators
	chars['+'] = TK_PLUS;
	chars['-'] = TK_MINUS;
	chars['/'] = TK_SLASH;
	chars['*'] = TK_ASTERISK;
	chars['%'] = TK_PERCENT;
	chars['&'] = TK_AMPERSAND;
	chars['|'] = TK_PIPE;
	chars['^'] = TK_CARET;
	chars['='] = TK_EQUAL;
	chars['!'] = TK_EXCLAMATION;
	chars['<'] = TK_LESSER;
	chars['>'] = TK_GREATER;

	lex = lex_cached;

	return lex_cached(cx, out_tk);
}

lex_pfn lex = lex_initial;

