#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "parse.h"
#include "tk.h"
#include "ast.h"
#include "err.h"
#include "type.h"
#include "sym.h"

static inline INLINE
const Token* peek(ParseCtx* cx, usz offs) {
	static Token eof_tk = (Token) { TK_INVALID, 0, 0, 0 };

	if (cx->tk_it + offs >= cx->tk_len)
		return &eof_tk;

	return &cx->tk_data[cx->tk_it + offs];
}

static inline INLINE
const Token* consume(ParseCtx* cx) {
	if (cx->tk_it >= cx->tk_len)
		err("%s: Unexpected end of file", cx->file_path);

	return &cx->tk_data[cx->tk_it++];
}

static inline INLINE
const Token* consume_type(ParseCtx* cx, TokenType type) {
	if (cx->tk_it >= cx->tk_len)
		err("%s: Unexpected end of file", cx->file_path);

	const Token* tk = &cx->tk_data[cx->tk_it++];
	if (tk->type != type)
		err("%s:%zu: Expected %s, got '%.*s'", cx->file_path, tk->line_index + 1,  tk_type_str(type), tk->len, &cx->char_data[tk->start]);
	return tk;
}

static
unsigned long tk_to_int(ParseCtx* cx, const Token* tk) {
	const char* c_it = &cx->char_data[tk->start + tk->len - 1];

	unsigned long mult = 1;
	unsigned long val = 0;

	for (usz i = 0; i < tk->len; ++i) {
		val += (*(c_it--) - '0') * mult;
		mult *= 10;
	}

	return val;
}

static
double tk_to_float(ParseCtx* cx, const Token* tk) {
	return 5.5f; // TODO: Fix this
}

Expression parse_primary(ParseCtx* cx) {
	Token tk = *peek(cx, 0);

	switch (tk.type) {
		case TK_LEFT_PARENTH: {
			consume(cx);
			if (peek(cx, 0)->type == TK_RIGHT_PARENTH) { // If it is a function definition
				consume(cx);
				Expression expr = make_expr(EXPR_LAMBDA);
				parse_compound(cx);
				return expr;
			}
			else { // If it is not a function definition
				Expression expr = parse_expr(cx);
				consume_type(cx, TK_RIGHT_PARENTH);
				return expr;
			}
		}	break;

		case TK_INTEGER: {
			const Token* tk = consume(cx);
			Expression expr = make_expr(EXPR_INTEGER);
			expr.lit_int = tk_to_int(cx, tk);
			return expr;
		}	break;

		case TK_FLOAT: {
			const Token* tk = consume(cx);
			Expression expr = make_expr(EXPR_FLOAT);
			expr.lit_float = tk_to_float(cx, tk);
		}

		default:
			err("%s:%zu: Unexpected token '%.*s' in expression", cx->file_path, tk.line_index + 1, tk.len, &cx->char_data[tk.start]);
	}
}

typedef
struct BinaryOperator {
	u8 tk_type;
	u8 expr_type;
	u8 precedence;
} BinaryOperator;

static const BinaryOperator bin_operators[] = {
	{ TK_PLUS,      EXPR_ADD,       4 },
	{ TK_MINUS,     EXPR_SUBTRACT,  4 },
	{ TK_ASTERISK,  EXPR_MULTIPLY,  3 },
	{ TK_SLASH,     EXPR_DIVIDE,    3 },
};

static
const BinaryOperator* find_bin_op(TokenType tk_type) {
	for (usz i = 0; i < sizeof(bin_operators) / sizeof(BinaryOperator); ++i) {
		if (bin_operators[i].tk_type == tk_type)
			return &bin_operators[i];
	}
	return NULL;
}

static
Expression parse_expr_binary(ParseCtx* cx, int prev_precd) {
	Expression left = parse_primary(cx);

	const BinaryOperator* op = find_bin_op(peek(cx, 0)->type);
	while (op && op->precedence < prev_precd) {
		consume(cx);
		Expression right = parse_expr_binary(cx, op->precedence);
		left = make_bin_expr(op->expr_type, &left, &right);
		op = find_bin_op(peek(cx, 0)->type);
	}

	return left;
}

Expression parse_expr(ParseCtx* cx) {
	return parse_expr_binary(cx, 99999999);
}

void parse_compound(ParseCtx* cx) {
	consume_type(cx, TK_LEFT_BRACE);

	for (;;) {
		Token tk = *peek(cx, 0);

		if (tk.type == TK_INVALID)
			err("%s: Expected closing '}' before end of file", cx->file_path);
		if (tk.type == TK_RIGHT_BRACE) {
			consume(cx);
			break;
		}

		parse_stmt(cx);
	}
}

void parse_stmt(ParseCtx* cx) {
	Token tk = *peek(cx, 0);

	switch (tk.type) {
		case TK_KW_LET: {
			consume(cx);
			const Token* name_tk = consume_type(cx, TK_IDENTIFIER);
			consume_type(cx, TK_DOUBLE_COLON);

			Expression expr = parse_expr(cx);
			// gen_expr(&expr);
			free_expr_children(&expr);
		}	break;

		case TK_KW_RETURN: {
			consume(cx);
			consume_type(cx, TK_SEMICOLON);
		}	break;

		case TK_SEMICOLON: {
			consume(cx);
		}	break;

		case TK_LEFT_BRACE: {
			return parse_compound(cx);
		}	break;

		default: {
			TypeHandle type_hnd;
			if (parse_type(cx, &type_hnd)) {
				const Token* name_tk = consume_type(cx, TK_IDENTIFIER);
				consume_type(cx, TK_DOUBLE_COLON);

				Expression expr = parse_expr(cx);
				// gen_expr(&expr);
				free_expr_children(&expr);
				break;
			}

			Expression expr = parse_expr(cx);
			// gen_expr(&expr);
			free_expr_children(&expr);
		}	break;
	}
}

b8 parse_type(ParseCtx* cx, TypeHandle* ret_hnd) {
	Token tk = *peek(cx, 0);

	switch (tk.type) {
		case TK_IDENTIFIER: {
			TypeHandle type_hnd = find_type(cx->syms, LSTR(&cx->char_data[tk.start], tk.len));
			if (!type_handle_valid(type_hnd))
				return 0;

			consume(cx);
			if (ret_hnd)
				*ret_hnd = type_hnd;
			return 1;
		}	break;

		default:
			return 0;
	}
}

void parse(ParseCtx* cx) {
	while (cx->tk_it < cx->tk_len)
		parse_stmt(cx);
}
