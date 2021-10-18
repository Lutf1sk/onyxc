#include "tk.h"

lstr_t tk_type_str(tk_stype_t stype) {
	switch (stype) {
#define TK_OP(x) case TK_##x: return CLSTR(#x);
	FOR_EACH_TK()
#undef TK_OP
	default:
		return CLSTR("INVALID_TOKEN");
	}
}

usz unescape_str(char* out, lstr_t str) {
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
		case 'n': *it++ = 0x0A; break;
		case '"': *it++ = '"'; break;
		case '\'': *it++ = '\''; break;
		default:
			lt_ferrf("Unknown escape sequence '%S'\n", LSTR(&str.str[start], i - start)); // TODO: Proper errors
		}
	}
	return it - out;
}

