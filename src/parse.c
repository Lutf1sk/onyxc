#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "parse.h"
#include "tk.h"
#include "ast.h"
#include "err.h"
#include "type.h"
#include "sym.h"
#include "intermediate.h"

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

static inline INLINE
void emit(ParseCtx* cx, Instr instr) {
	if (cx->curr_func == -1)
		err("%s: Code can only be generated inside a function", cx->file_path);
	add_instr(&cx->funcs[cx->curr_func], instr);
}

static inline INLINE
b8 curr_scope_global(ParseCtx* cx) {
    return cx->syms->next != NULL;
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
	(void)cx;
	(void)tk;
	return 5.5f; // TODO: Fix this
}

static
usz add_intermediate_func(ParseCtx* cx, const IntermediateFunc* func) {
	if (cx->func_count >= cx->func_alloc_count) {
		// Double the size and reallocate the array
		// OR if the array has not been allocated yet, allocate space for 1024 instructions.
		if (cx->func_alloc_count)
			cx->func_alloc_count *= 2;
		else
			cx->func_alloc_count = 1024;

		cx->funcs = realloc(cx->funcs, cx->func_alloc_count * sizeof(IntermediateFunc));
		assert(cx->funcs);
	}

	cx->funcs[cx->func_count] = *func;
	return cx->func_count++;
}

static
usz gen_expr(ParseCtx* cx, Expression* expr) {
	assert(expr->type != EXPR_INVALID);

    usz left_res;
    usz right_res;

	switch (expr->type) {
    case EXPR_ADD:
        assert(expr->child_count == 2);
        left_res = gen_expr(cx, &expr->children[0]);
        right_res = gen_expr(cx, &expr->children[1]);
        emit(cx, make_instr_u(make_instr_op(IN_ADD, ISZ_64, ITP_UINT), 0, left_res, right_res));
        break;

    case EXPR_SUBTRACT:
        assert(expr->child_count == 2);
        left_res = gen_expr(cx, &expr->children[0]);
        right_res = gen_expr(cx, &expr->children[1]);
        emit(cx, make_instr_u(make_instr_op(IN_SUB, ISZ_64, ITP_UINT), 0, left_res, right_res));
        break;

    case EXPR_MULTIPLY:
        assert(expr->child_count == 2);
        left_res = gen_expr(cx, &expr->children[0]);
        right_res = gen_expr(cx, &expr->children[1]);
        emit(cx, make_instr_u(make_instr_op(IN_MUL, ISZ_64, ITP_UINT), 0, left_res, right_res));
        break;

    case EXPR_DIVIDE:
        assert(expr->child_count == 2);
        left_res = gen_expr(cx, &expr->children[0]);
        right_res = gen_expr(cx, &expr->children[1]);
        emit(cx, make_instr_u(make_instr_op(IN_DIV, ISZ_64, ITP_UINT), 0, left_res, right_res));
        break;

    case EXPR_LABEL:
        emit(cx, make_instr_u(make_instr_op(IN_LOAD_LABEL, ISZ_64, ITP_UINT), 0, 0, 0));
        break;

    case EXPR_INTEGER:
        emit(cx, make_instr_u(make_instr_op(IN_LOAD_LIT, ISZ_64, ITP_UINT), 0, 0, 0));
        break;

    case EXPR_FLOAT:
        emit(cx, make_instr_u(make_instr_op(IN_LOAD_LIT, ISZ_64, ITP_FLOAT), 0, 0, 0));
        break;

    default:
        err("Invalid expression type\n");
	}

	return 0;
}

Expression parse_primary(ParseCtx* cx) {
	Token tk = *peek(cx, 0);

	switch (tk.type) {
		case TK_LEFT_PARENTH: {
			consume(cx);
			if (peek(cx, 0)->type == TK_RIGHT_PARENTH) { // If it is a function definition
                consume(cx);
                IntermediateFunc new_func = make_intermediate_func();

                usz last_func = cx->curr_func;
                cx->curr_func = add_intermediate_func(cx, &new_func);
                parse_compound(cx);
                cx->curr_func = last_func;

                Expression expr = make_expr(EXPR_LABEL);
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

static
void parse_var_def(ParseCtx* cx, TypeHandle type_hnd) {
    const Token* name_tk = consume_type(cx, TK_IDENTIFIER);

    isz last_func = cx->curr_func;

    b8 is_glob = cx->curr_func == -1;
    b8 is_const = peek(cx, 0)->type == TK_DOUBLE_COLON;

    if (is_const)
        consume_type(cx, TK_DOUBLE_COLON);
    else
        consume_type(cx, TK_EQUAL);

    if (is_glob || is_const) {
        IntermediateFunc glob_init_func = make_intermediate_func();
        cx->curr_func = add_intermediate_func(cx, &glob_init_func);
    }

    Expression expr = parse_expr(cx);
    gen_expr(cx, &expr);
    free_expr_children(&expr);

    if (!type_handle_valid(type_hnd))
        type_hnd = expr.datatype;

    SymbolType sym_type = is_const ? SM_CONSTANT : (is_glob ? SM_GLOBAL_VAR : SM_LOCAL_VAR);
    usz offs = add_symbol(cx->syms, sym_type, LSTR(&cx->char_data[name_tk->start], name_tk->len), type_hnd);

    if (is_glob || is_const) {
        cx->syms->syms[offs].init_func = cx->curr_func;
        cx->curr_func = last_func;
    }
}

void parse_stmt(ParseCtx* cx) {
	Token tk = *peek(cx, 0);

	switch (tk.type) {
		case TK_KW_LET: {
			consume(cx);
            parse_var_def(cx, INVALID_TYPE);
		}	break;

		case TK_KW_RETURN: {
			if (cx->curr_func == -1)
				err("%s:%zu: Cannot return from global scope", cx->file_path, tk.line_index + 1, tk.len);

			emit(cx, make_instr_u(make_instr_op(IN_RET, ISZ_64, 0), 0, 0, 0));

			consume(cx);
			consume_type(cx, TK_SEMICOLON);
		}	break;

		case TK_SEMICOLON: {
			consume(cx);
		}	break;

		case TK_LEFT_BRACE: {
			if (cx->curr_func == -1)
				err("%s:%zu: Nested global scopes are not supported", cx->file_path, tk.line_index + 1, tk.len);

			return parse_compound(cx);
		}	break;

		default: {
			TypeHandle type_hnd;
			if (parse_type(cx, &type_hnd)) {
                parse_var_def(cx, type_hnd);
				break;
			}

			if (cx->curr_func == -1)
				err("%s:%zu: Unexpected token '%.*s'", cx->file_path, tk.line_index + 1, tk.len, &cx->char_data[tk.start]);

			Expression expr = parse_expr(cx);
			gen_expr(cx, &expr);
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
