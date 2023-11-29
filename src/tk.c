#include "tk.h"
#include "err.h"
#include "textattrib.h"

#include <lt/debug.h>
#include <lt/strstream.h>

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
	LT_ASSERT_NOT_REACHED();
	return 0; // TODO: Error checking
}

lstr_t unescape_str(tk_t* tk, lt_alloc_t* alloc) {
	lt_strstream_t stream, *s = &stream;
	LT_ASSERT(lt_strstream_create(s, alloc) == LT_SUCCESS);

	lstr_t str = LSTR(tk->str.str + 1, tk->str.len - 2);

	for (usz i = 0; i < str.len;) {
		usz start = i;

		char c = str.str[i++];
		if (c != '\\' || i == str.len) {
			lt_strstream_writec(s, c); // !! This is much slower than necessary
			continue;
		}

		c = str.str[i++];
		switch (c) {
		case 'r': lt_strstream_writec(s, '\r'); break;
		case 'v': lt_strstream_writec(s, '\v'); break;
		case 'b': lt_strstream_writec(s, '\b'); break;
		case 't': lt_strstream_writec(s, '\t'); break;
		case 'n': lt_strstream_writec(s, '\n'); break;
		case '"': lt_strstream_writec(s, '"'); break;
		case '\'': lt_strstream_writec(s, '\''); break;
		case '\\': lt_strstream_writec(s, '\\'); break;
		case 'x': {
			if (i >= str.len - 1)
				ferr(A_BOLD"'\\x'"A_RESET" escape sequence must be followed by two hexadecimal digits", *tk);
			u8 b = hex_char(str.str[i++]) << 4;
			b |= hex_char(str.str[i++]);
			lt_strstream_writec(s, b);
			break;
		}
		default:
			ferr("unknown escape sequence "A_BOLD"'%S'"A_RESET, *tk, LSTR(&str.str[start], i - start));
		}
	}

	return stream.str;
}

