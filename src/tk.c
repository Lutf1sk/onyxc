#include "tk.h"
#include "err.h"
#include "textattrib.h"

lstr_t tk_type_str(tk_stype_t stype) {
	switch (stype) {
#define TK_OP(x) case TK_##x: return CLSTR(#x);
	FOR_EACH_TK()
#undef TK_OP
	default:
		return CLSTR("INVALID_TOKEN");
	}
}

static
u8 hex_char(u8 c) {
	if (c >= 'A' && c <= 'Z')
		return (c - 'A' + 10);
	if (c >= 'a' && c <= 'z')
		return (c - 'a' + 10);
	if (c >= '0' && c <= '9')
		return (c - '0');
	return 0;
}

usz unescape_str(lex_ctx_t* cx, char* out, tk_t* tk) {
	lstr_t str = LSTR(tk->str.str + 1, tk->str.len - 2);

	char* it = out;
	for (usz i = 0; i < str.len;) {
		usz start = i;

		char c = str.str[i++];
		if (c != '\\' || i == str.len) {
			*it++ = c;
			continue;
		}

		c = str.str[i++];
		switch (c) {
		case 'r': *it++ = '\r'; break;
		case 'v': *it++ = '\v'; break;
		case 'b': *it++ = '\b'; break;
		case 't': *it++ = '\t'; break;
		case 'n': *it++ = '\n'; break;
		case '"': *it++ = '"'; break;
		case '\'': *it++ = '\''; break;
		case '\\': *it++ = '\\'; break;
		case 'x': {
			if (i >= str.len - 1)
				ferr(A_BOLD"'\\x'"A_RESET" escape sequence must be followed by two hexadecimal digits", cx, *tk);
			*it = hex_char(str.str[i++]) << 4;
			*it++ |= hex_char(str.str[i++]);
			break;
		}
		default:
			ferr("unknown escape sequence "A_BOLD"'%S'"A_RESET, cx, *tk, LSTR(&str.str[start], i - start));
		}
	}
	return it - out;
}

