#include "tk.h"

const char* tk_type_str(TokenType type) {
    switch (type) {
#define TOKEN_OP(x) case TK_##x: return #x;
        FOR_EACH_TOKEN()
#undef TOKEN_OP
        default:
            return "INVALID";
    }
}
