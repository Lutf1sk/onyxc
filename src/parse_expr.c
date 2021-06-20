// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#include <assert.h>

#include "parse.h"
#include "ast.h"
#include "intermediate.h"
#include "parse_helpers.h"

static
usz gen_bin_expr(ParseCtx* cx, InstrSize sz, InstrOpNum op, Expression* expr) {
    assert(expr->child_count == 2);
    usz left_res = gen_expr(cx, &expr->children[0]);
    usz right_res = gen_expr(cx, &expr->children[1]);
    usz ret_reg = alloc_register(cx);
    Instr instr = make_instr_r(make_instr_op(op, sz, ITP_UINT),ret_reg, left_res, right_res);
    emit(cx, instr);
    return ret_reg;
}

usz gen_expr(ParseCtx* cx, Expression* expr) {
    assert(expr->type != EXPR_INVALID);

    InstrSize instr_sz = ISZ_64;

    switch (expr->type) {
    case EXPR_ADD:      return gen_bin_expr(cx, instr_sz, IN_ADD, expr);
    case EXPR_SUBTRACT: return gen_bin_expr(cx, instr_sz, IN_SUB, expr);
    case EXPR_MULTIPLY: return gen_bin_expr(cx, instr_sz, IN_MUL, expr);
    case EXPR_DIVIDE:   return gen_bin_expr(cx, instr_sz, IN_DIV, expr);

    case EXPR_LAMBDA: {
        usz ret_reg = alloc_register(cx);
        Instr instr = make_instr(make_instr_op(IN_LOAD_FUNC, instr_sz, ITP_UINT), ret_reg);
        instr.func_offs = expr->func_offs;
        emit(cx, instr);
        return ret_reg;
    }   break;

    case EXPR_INTEGER: {
        usz ret_reg = alloc_register(cx);
        Instr instr = make_instr(make_instr_op(IN_LOAD_LIT, instr_sz, ITP_UINT), ret_reg);
        instr.lit_uint = expr->lit_uint;
        emit(cx, instr);
        return ret_reg;
    }   break;

    case EXPR_FLOAT: {
        usz ret_reg = alloc_register(cx);
        Instr instr = make_instr(make_instr_op(IN_LOAD_LIT, instr_sz, ITP_FLOAT), ret_reg);
        instr.lit_float = expr->lit_float;
        emit(cx, instr);
        return ret_reg;
    }   break;

    case EXPR_FUNC_CALL: {
        for (usz i = 1; i < expr->child_count; ++i) {
            usz arg_reg = gen_expr(cx, &expr->children[i]);
            Instr arg_instr = make_instr(make_instr_op(IN_STORE_ARG, instr_sz, ITP_UINT), arg_reg);
            arg_instr.lit_uint = i - 1;
            emit(cx, arg_instr);
        }

        usz func_reg = gen_expr(cx, &expr->children[0]);
        Instr func_instr = make_instr(make_instr_op(IN_CALL, instr_sz, ITP_UINT), func_reg);
        emit(cx, func_instr);

        usz ret_reg = alloc_register(cx);
        Instr ret_instr = make_instr(make_instr_op(IN_LOAD_RETVAL, instr_sz, ITP_UINT), ret_reg);
        emit(cx, ret_instr);
        return ret_reg;
    }   break;

    default:
        err("Invalid expression type\n");
    }

    err("Internal compiler error");
}

static
Expression parse_primary(ParseCtx* cx) {
    Token tk = *peek(cx, 0);

    switch (tk.type) {
    case TK_LEFT_PARENTH: {
        consume(cx);
        // If it is a function definition
        if (peek(cx, 0)->type == TK_RIGHT_PARENTH) {
            consume(cx);
            IntermediateFunc new_func = make_intermediate_func();

            usz last_func = cx->curr_func;
            usz new_func_offs = add_intermediate_func(cx, &new_func);

            cx->curr_func = new_func_offs;
            parse_compound(cx);
            cx->curr_func = last_func;

            Expression expr = make_expr(EXPR_LAMBDA, u64_hnd);
            expr.func_offs = new_func_offs;
            return expr;
        }
        else { // If it is not a function definition
            Expression expr = parse_expr(cx);
            consume_type(cx, TK_RIGHT_PARENTH);
            return expr;
        }
    }   break;

    case TK_INTEGER: {
        const Token* tk = consume(cx);
        Expression expr = make_expr(EXPR_INTEGER, u64_hnd);
        expr.lit_sint = tk_to_int(cx, tk);
        return expr;
    }   break;

    case TK_FLOAT: {
        const Token* tk = consume(cx);
        Expression expr = make_expr(EXPR_FLOAT, u64_hnd);
        expr.lit_float = tk_to_float(cx, tk);
    }

    default:
        err("%s:%zu: Unexpected token '%.*s' in expression", cx->file_path, tk.line_index + 1, tk.len, &cx->char_data[tk.start]);
    }
}

static
Expression parse_sfx(ParseCtx* cx, Expression child) {
    Token tk = *peek(cx, 0);

    switch (tk.type) {
    case TK_LEFT_PARENTH:
        consume(cx);
        consume_type(cx, TK_RIGHT_PARENTH);
        Expression expr = make_un_expr(EXPR_FUNC_CALL, u64_hnd, &child);
        return parse_sfx(cx, expr);
        break;

    default:
        return child;
    }
}

static
Expression parse_pfx(ParseCtx* cx) {
    Token tk = *peek(cx, 0);

    switch (tk.type) {
    default:
        return parse_sfx(cx, parse_primary(cx));
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
    Expression left = parse_pfx(cx);

    const BinaryOperator* op = find_bin_op(peek(cx, 0)->type);
    while (op && op->precedence < prev_precd) {
        consume(cx);
        Expression right = parse_expr_binary(cx, op->precedence);
        left = make_bin_expr(op->expr_type, u64_hnd, &left, &right);
        op = find_bin_op(peek(cx, 0)->type);
    }

    return left;
}

Expression parse_expr(ParseCtx* cx) {
    return parse_expr_binary(cx, 99999999);
}
