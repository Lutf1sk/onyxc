
OUT = onyxc

OBJS = \
	src/main.o \
	src/tk.o \
	src/jit.o \
	src/err.o \
	src/gen.o \
	src/interm.o \
	src/lex.o \
	src/segment.o \
	src/expr_ast.o \
	src/stmt_ast.o \
	src/parse.o \
	src/parse_stmt.o \
	src/parse_expr.o \
	src/parse_type.o \
	src/type_convertions.o \
	src/type.o \
	src/symtab.o \
	src/amd64/amd64.o \
	src/amd64/asm.o \
	src/amd64/ops.o \
	src/amd64/regs.o \
	src/amd64/common.o \
	src/amd64/elf.o \
	src/amd64/lbl.o

DEPS = $(patsubst %.o,%.deps,$(OBJS))

CC = cc
CC_FLAGS += -fno-omit-frame-pointer -O2 -fmax-errors=3 -Wall -Werror -Wno-strict-aliasing -Wno-error=unused-variable -Wno-unused-function -Ilt/include/ -Wno-pedantic -std=c11

LNK = cc
LNK_FLAGS += -o $(OUT) -rdynamic -g
LNK_LIBS += -lpthread -ldl -lm

ifdef DEBUG
	CC_FLAGS += -g
endif

ifdef UBSAN
	LNK_LIBS += -lubsan
	CC_FLAGS += -fsanitize=undefined
endif

LT_PATH = lt/bin/release/lt.a

all: $(LT_PATH) $(OUT)

$(LT_PATH):
	make -C lt/

run: all
	./$(OUT) test.nyx

install: all
	@-rm -rf /usr/lib/onyxc/

	cp $(OUT) /usr/local/bin/

	mkdir -p /usr/lib/onyxc
	cp -r std /usr/lib/onyxc/

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

.PHONY: all clean run sync install
