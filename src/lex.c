#include "lex.h"
#include "err.h"
#include "textattrib.h"
#include "tk_ctype.h"

#include <lt/str.h>
#include <lt/mem.h>
#include <lt/io.h>
#include <lt/ctype.h>

lex_ctx_t* lex_file(lt_arena_t* arena, char* path, tk_t* path_tk) {
	// Read source file
	lt_file_t* fp = lt_file_open(arena, path, LT_FILE_R, 0);
	if (!fp) {
		if (path_tk)
			ferr("failed to open '%s'", *path_tk, path);
		lt_ferrf("failed to open '%s'\n", path);
	}

	usz size = lt_file_size(fp);
	char* data = lt_arena_reserve(arena, size + 2);
	if (lt_file_read(fp, data + 1, size) != size)
		lt_ferrf("failed to read from '%s'\n", path);
	data[0] = 0;
	data[size + 1] = 0;
	lt_file_close(fp);

	// Lex data
	lex_ctx_t* lex_cx = lt_arena_reserve(arena, sizeof(lex_ctx_t));
	lex_cx->path = path;
	lex_cx->data = LSTR(data + 1, size);
	lex_cx->count = lex(lex_cx, lt_arena_reserve(arena, 0));
	lex_cx->tk_data = lt_arena_reserve(arena, lex_cx->count * sizeof(tk_t));

	return lex_cx;
}

static
tk_stype_t identifier_type(lstr_t str) {
	if (str.len < 2)
		return TK_IDENTIFIER;

	switch (*str.str) {
	case 'a':
		if (lt_lstr_eq(str, CLSTR("alignof"))) return TK_KW_ALIGNOF;
		break;

	case 'b':
		if (lt_lstr_eq(str, CLSTR("break"))) return TK_KW_BREAK;
		break;

	case 'c':
		if (lt_lstr_eq(str, CLSTR("continue"))) return TK_KW_CONTINUE;
		if (lt_lstr_eq(str, CLSTR("case"))) return TK_KW_CASE;
		break;

	case 'd':
		if (lt_lstr_eq(str, CLSTR("do"))) return TK_KW_DO;
		if (lt_lstr_eq(str, CLSTR("def"))) return TK_KW_DEF;
		if (lt_lstr_eq(str, CLSTR("default"))) return TK_KW_DEFAULT;
		break;

	case 'e':
		if (lt_lstr_eq(str, CLSTR("explicit"))) return TK_KW_EXPLICIT;
		if (lt_lstr_eq(str, CLSTR("else"))) return TK_KW_ELSE;
		if (lt_lstr_eq(str, CLSTR("elif"))) return TK_KW_ELIF;
		if (lt_lstr_eq(str, CLSTR("enum"))) return TK_KW_ENUM;
		break;

	case 'f':
		if (lt_lstr_eq(str, CLSTR("for"))) return TK_KW_FOR;
		break;

	case 'g':
		if (lt_lstr_eq(str, CLSTR("goto"))) return TK_KW_GOTO;
		break;

	case 'i':
		if (lt_lstr_eq(str, CLSTR("implicit"))) return TK_KW_IMPLICIT;
		if (lt_lstr_eq(str, CLSTR("import"))) return TK_KW_IMPORT;
		if (lt_lstr_eq(str, CLSTR("if"))) return TK_KW_IF;
		break;

	case 'l':
		if (lt_lstr_eq(str, CLSTR("let"))) return TK_KW_LET;
		break;

	case 'n':
		if (lt_lstr_eq(str, CLSTR("null"))) return TK_KW_NULL;
		break;

	case 'r':
		if (lt_lstr_eq(str, CLSTR("return"))) return TK_KW_RETURN;
		break;

	case 's':
		if (lt_lstr_eq(str, CLSTR("switch"))) return TK_KW_SWITCH;
		if (lt_lstr_eq(str, CLSTR("struct"))) return TK_KW_STRUCT;
		if (lt_lstr_eq(str, CLSTR("syscall"))) return TK_KW_SYSCALL;
		if (lt_lstr_eq(str, CLSTR("sizeof"))) return TK_KW_SIZEOF;
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

#define emit(x) (out_tk[tk_count++] = TK(cx, (x), LSTR(&cx->data.str[tk_start], it - tk_start), line_index))

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
			if (c == '/') { while (data[++it] != '\n') ; }
			else if (c == '*') {
				while ((c = data[it++])) {
					if (c == '*' && data[it] == '/') {
						++it;
						break;
					}
				}
			}
			else if (c == '=') { ++it; emit(TK_SLASH_EQUAL); }
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

		case TK_NUMBER: {
			if (c == '0') {
				c = data[it];
				if (c == 'x' || c == 'X') { // Hexadecimal
					++it;
					while (is_hex_digit(data[it]))
						++it;
				}
				else if (c == 'b' || c == 'B') { // Binary
					++it;
					while (data[it] == '0' || data[it] == '1')
						++it;
				}
				else
					goto parse_noprefix_end;
			}
			else { // No prefix
				while (is_digit(data[it]))
					++it;
				c = data[it];
			parse_noprefix_end:
				if (c == '.' && is_digit(data[it + 1])) { // Float
					++it;
					while (is_digit(data[it]))
						++it;
					if (data[it] == 'f' || data[it] == 'F')
						++it;
				}
				else {
					c = lt_to_lower(data[it]);
					if (c == 'u' || c == 'i' || c == 'f' || c == 'k' || c == 'm' || c == 'g' || c == 't')
						++it;
				}
			}
			emit(TK_NUMBER);
		}	break;

		case TK_DOT: c = data[it];
			if (c == '.') { ++it; emit(TK_DOUBLE_DOT); }
			else emit(TK_DOT);
			break;

		case TK_CHAR: {
			for (;;) {
				c = data[it++];
				if ((u8)c < 32 && c != '\t')
					ferr("unterminated character literal", TK(cx, TK_CHAR, LSTR(&data[tk_start], it - tk_start - 1), line_index));
				if (c == '\'')
					break;
				if (c == '\\')
					++it;
			}
			emit(TK_CHAR);
		}	break;

		case TK_STRING: {
			for (;;) {
				c = data[it++];
				if ((u8)c < 32 && c != '\t')
					ferr("unterminated string literal", TK(cx, TK_STRING, LSTR(&data[tk_start], it - tk_start - 1), line_index));
				if (c == '"')
					break;
				if (c == '\\')
					++it;
			}
			emit(TK_STRING);
		}	break;

		case TK_IDENTIFIER: {
			while(is_ident_body(data[it]))
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
			ferr("unexpected character "A_BOLD"'%c'"A_RESET, TK(cx, TK_INVALID, LSTR(&data[tk_start], it - tk_start), line_index), c);

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
		chars[i] = TK_NUMBER;

	chars['\''] = TK_CHAR;
	chars['"'] = TK_STRING;

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
	chars['~'] = TK_TILDE;
	chars['='] = TK_EQUAL;
	chars['!'] = TK_EXCLAMATION;
	chars['<'] = TK_LESSER;
	chars['>'] = TK_GREATER;

	lex = lex_cached;

	return lex_cached(cx, out_tk);
}

lex_pfn lex = lex_initial;

