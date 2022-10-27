#include "err.h"
#include "lex.h"
#include "textattrib.h"

#include <lt/io.h>

LT_NORETURN
void exit(int code);

static
lstr_t line_find_beg(tk_t* tk) {
	lstr_t line_beg;
	for (char* it = tk->str.str;; --it) {
		if (!*it || *it == '\n') {
			line_beg.str = it + 1;
			break;
		}
	}
	line_beg.len = tk->str.str - line_beg.str;
	return line_beg;
}

static
lstr_t line_find_end(tk_t* tk) {
	lstr_t line_end;
	for (char* it = &tk->str.str[tk->str.len];; ++it) {
		if (!*it || *it == '\n') {
			line_end.len = it - (tk->str.str + tk->str.len);
			break;
		}
	}
	line_end.str = &tk->str.str[tk->str.len];
	return line_end;
}

void ferr(char* fmt, tk_t tk, ...) {
	lstr_t line_beg = line_find_beg(&tk), line_end = line_find_end(&tk);

	const usz line = tk.line_index + 1;

	lt_printf(A_BOLD"%s:%uz:%uz: "A_RED"error:"A_RESET" ", tk.cx->path, line, line_beg.len);

	va_list args;
	va_start(args, 0);
	lt_vfprintf(lt_stdout, fmt, args);
	va_end(args);

	lt_printf("\n %uz| %S"A_RED A_BOLD"%S"A_RESET"%S\n", line, line_beg, tk.str, line_end);

	exit(1);
}

void werr(char* fmt, tk_t tk, ...) {
	lstr_t line_beg = line_find_beg(&tk), line_end = line_find_end(&tk);

	const usz line = tk.line_index + 1;

	lt_printf(A_BOLD"%s:%uz:%uz: "A_MAGENTA"warning:"A_RESET" ", tk.cx->path, line, line_beg.len);

	va_list args;
	va_start(args, 0);
	lt_vfprintf(lt_stdout, fmt, args);
	va_end(args);

	lt_printf("\n %uz| %S"A_MAGENTA A_BOLD"%S"A_RESET"%S\n", line, line_beg, tk.str, line_end);
}

