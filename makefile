
OUT = onyxc

OBJS = \
	src/main.o \
	src/tk.o \
	src/lex.o \
	src/expr_ast.o \
	src/stmt_ast.o \
	src/parse.o \
	src/parse_stmt.o \
	src/parse_expr.o \
	src/parse_type.o \
	src/type_convertions.o \
	src/type.o \
	src/symtab.o

DEPS = $(patsubst %.o,%.deps,$(OBJS))

CC = cc
CC_FLAGS += -Wall -Werror -Ilt/include/ -Wno-pedantic -std=c11

LNK = cc
LNK_FLAGS += -o $(OUT) -rdynamic -g
LNK_LIBS += -O3 -lm -lX11 -lX11-xcb -lxcb -lxcb-randr -lxcb-ewmh -lpthread -ldl

ifdef DEBUG_SYMS
	CC_FLAGS += -g
endif

ifdef USE_UBSAN
	LNK_LIBS += -lubsan
	CC_FLAGS += -fsanitize=undefined
endif

LT_PATH = lt/bin/lt.a

all: $(LT_PATH) $(OUT)

$(LT_PATH):
	make -C lt/

run: all
	./$(OUT) test.nyx

prof: all
	valgrind --dump-instr=yes --tool=callgrind ./$(OUT) test.nyx
	kcachegrind ./callgrind.out*
	rm ./callgrind.out.*

$(OUT):	$(OBJS)
	$(LNK) $(LNK_FLAGS) $(OBJS) $(LT_PATH) $(LNK_LIBS)

%.o: %.c makefile
	$(CC) $(CC_FLAGS) -MM -MT $@ -MF $(patsubst %.o,%.deps,$@) $<
	$(CC) $(CC_FLAGS) -c $< -o $@

-include $(DEPS)

clean:
	rm $(OBJS) $(DEPS)

.PHONY: all clean run sync
