#include "parse.h"
#include "tk.h"
#include "expr_ast.h"
#include "stmt_ast.h"

operator_t operators[TK_MAX];
operator_t sfx_operators[TK_MAX];
operator_t pfx_operators[TK_MAX];

static
stmt_t* parse_cached(parse_ctx_t* cx) {
	cx->it = 0;

	stmt_t* root = NULL;
	stmt_t** current = &root;
	while (cx->it < cx->count) {
		stmt_t* new = parse_stmt(cx);
		if (!new)
			continue;
		*current = new;
		current = &new->next;
	}
	return root;
}

static
stmt_t* parse_initial(parse_ctx_t* cx) {
	memset(operators, 0xFF, TK_MAX * sizeof(operator_t));
	operators[TK_ASTERISK]			= (operator_t){ TK_ASTERISK,		3,	EXPR_MULTIPLY,			OP_ASSOC_LEFT };
	operators[TK_SLASH]				= (operator_t){ TK_SLASH,			3,	EXPR_DIVIDE,			OP_ASSOC_LEFT };
	operators[TK_PERCENT]			= (operator_t){ TK_PERCENT,			3,	EXPR_MODULO,			OP_ASSOC_LEFT };
	operators[TK_PLUS]				= (operator_t){ TK_PLUS,			4,	EXPR_ADD,				OP_ASSOC_LEFT };
	operators[TK_MINUS]				= (operator_t){ TK_MINUS,			4,	EXPR_SUBTRACT,			OP_ASSOC_LEFT };
	operators[TK_DOUBLE_LESSER]		= (operator_t){ TK_DOUBLE_LESSER, 	5,	EXPR_BIT_SHIFT_LEFT,	OP_ASSOC_LEFT };
	operators[TK_DOUBLE_GREATER]	= (operator_t){ TK_DOUBLE_GREATER,	5,	EXPR_BIT_SHIFT_RIGHT,	OP_ASSOC_LEFT };
	operators[TK_LESSER]			= (operator_t){ TK_LESSER,			6,	EXPR_LESSER,			OP_ASSOC_LEFT };
	operators[TK_GREATER]			= (operator_t){ TK_GREATER,			6,	EXPR_GREATER,			OP_ASSOC_LEFT };
	operators[TK_LESSER_EQUAL]		= (operator_t){ TK_LESSER_EQUAL,	6,	EXPR_LESSER_OR_EQUAL,	OP_ASSOC_LEFT };
	operators[TK_GREATER_EQUAL]		= (operator_t){ TK_GREATER_EQUAL,	6,	EXPR_GREATER_OR_EQUAL,	OP_ASSOC_LEFT };
	operators[TK_DOUBLE_EQUAL]		= (operator_t){ TK_DOUBLE_EQUAL,	7,	EXPR_EQUAL,				OP_ASSOC_LEFT };
	operators[TK_EXCLAMATION_EQUAL]	= (operator_t){ TK_EXCLAMATION_EQUAL,7,	EXPR_NOT_EQUAL,			OP_ASSOC_LEFT };
	operators[TK_AMPERSAND]			= (operator_t){ TK_AMPERSAND,		8,	EXPR_BIT_AND,			OP_ASSOC_LEFT };
	operators[TK_CARET]				= (operator_t){ TK_CARET,			9,	EXPR_BIT_XOR,			OP_ASSOC_LEFT };
	operators[TK_PIPE]				= (operator_t){ TK_PIPE,			10,	EXPR_BIT_OR,			OP_ASSOC_LEFT };
	operators[TK_DOUBLE_AMPERSAND]	= (operator_t){ TK_DOUBLE_AMPERSAND,11,	EXPR_LOGIC_AND,			OP_ASSOC_LEFT };
	operators[TK_DOUBLE_PIPE]		= (operator_t){ TK_DOUBLE_PIPE,		12,	EXPR_LOGIC_OR,			OP_ASSOC_LEFT };
	operators[TK_EQUAL] 			= (operator_t){ TK_EQUAL,			14,	EXPR_ASSIGN,			OP_ASSOC_RIGHT };

	operators[TK_PLUS_EQUAL]			= (operator_t){ TK_PLUS_EQUAL,			14,	EXPR_ADD_ASSIGN,			OP_ASSOC_RIGHT };
	operators[TK_MINUS_EQUAL]			= (operator_t){ TK_MINUS_EQUAL,			14,	EXPR_SUBTRACT_ASSIGN,		OP_ASSOC_RIGHT };
	operators[TK_ASTERISK_EQUAL]		= (operator_t){ TK_ASTERISK_EQUAL,		14,	EXPR_MULTIPLY_ASSIGN,		OP_ASSOC_RIGHT };
	operators[TK_SLASH_EQUAL]			= (operator_t){ TK_SLASH_EQUAL,			14,	EXPR_DIVIDE_ASSIGN,			OP_ASSOC_RIGHT };
	operators[TK_PERCENT_EQUAL]			= (operator_t){ TK_PERCENT_EQUAL,		14,	EXPR_MODULO_ASSIGN,			OP_ASSOC_RIGHT };
	operators[TK_DOUBLE_LESSER_EQUAL]	= (operator_t){ TK_DOUBLE_LESSER_EQUAL,	14,	EXPR_BIT_SHIFT_LEFT_ASSIGN,	OP_ASSOC_RIGHT };
	operators[TK_DOUBLE_GREATER_EQUAL]	= (operator_t){ TK_DOUBLE_GREATER_EQUAL,14,	EXPR_BIT_SHIFT_RIGHT_ASSIGN,OP_ASSOC_RIGHT };
	operators[TK_AMPERSAND_EQUAL]		= (operator_t){ TK_AMPERSAND_EQUAL,		14,	EXPR_BIT_AND_ASSIGN,		OP_ASSOC_RIGHT };
	operators[TK_CARET_EQUAL]			= (operator_t){ TK_CARET_EQUAL,			14,	EXPR_BIT_XOR_ASSIGN,		OP_ASSOC_RIGHT };
	operators[TK_PIPE_EQUAL]			= (operator_t){ TK_PIPE_EQUAL,			14,	EXPR_BIT_OR_ASSIGN,			OP_ASSOC_RIGHT };

	memset(pfx_operators, 0xFF, TK_MAX * sizeof(operator_t));
	pfx_operators[TK_DOUBLE_PLUS]	= (operator_t){ TK_DOUBLE_PLUS,		2, EXPR_PFX_INCREMENT,	OP_ASSOC_RIGHT };
	pfx_operators[TK_DOUBLE_MINUS]	= (operator_t){ TK_DOUBLE_MINUS,	2, EXPR_PFX_DECREMENT,	OP_ASSOC_RIGHT };
	pfx_operators[TK_MINUS]			= (operator_t){ TK_MINUS,			2, EXPR_NEGATE,			OP_ASSOC_RIGHT };
	pfx_operators[TK_EXCLAMATION]	= (operator_t){ TK_EXCLAMATION,		2, EXPR_LOGIC_NOT,		OP_ASSOC_RIGHT };
	pfx_operators[TK_TILDE]			= (operator_t){ TK_TILDE,			2, EXPR_BIT_NOT,		OP_ASSOC_RIGHT };
	pfx_operators[TK_ASTERISK] 		= (operator_t){ TK_ASTERISK,		2, EXPR_DEREFERENCE,	OP_ASSOC_RIGHT };
	pfx_operators[TK_AMPERSAND]		= (operator_t){ TK_AMPERSAND,		2, EXPR_REFERENCE,		OP_ASSOC_RIGHT };

	memset(sfx_operators, 0xFF, TK_MAX * sizeof(operator_t));
	sfx_operators[TK_DOUBLE_PLUS]	= (operator_t){ TK_DOUBLE_PLUS,		1, EXPR_SFX_INCREMENT,	OP_ASSOC_LEFT };
	sfx_operators[TK_DOUBLE_MINUS]	= (operator_t){ TK_DOUBLE_MINUS,	1, EXPR_SFX_DECREMENT,	OP_ASSOC_LEFT };
	sfx_operators[TK_LEFT_PARENTH]	= (operator_t){ TK_LEFT_PARENTH,	1, EXPR_CALL,			OP_ASSOC_LEFT };
	sfx_operators[TK_LEFT_BRACKET]	= (operator_t){ TK_LEFT_BRACKET,	1, EXPR_SUBSCRIPT,		OP_ASSOC_LEFT };
	sfx_operators[TK_DOT]			= (operator_t){ TK_DOT,				1, EXPR_MEMBER,			OP_ASSOC_LEFT };

	parse = parse_cached;
	return parse_cached(cx);
}

parse_pfn parse = parse_initial;

