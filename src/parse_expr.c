#include <assert.h>

#include "parse.h"
#include "ast.h"
#include "intermediate.h"
#include "parse_helpers.h"

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
