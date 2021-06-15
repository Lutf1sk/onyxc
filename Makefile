
OUT = onyxc

OBJS = \
	src/type.o \
	src/parse.o \
	src/parse_expr.o \
	src/ast.o \
	src/main.o \
	src/filehl.o \
	src/err.o \
	src/scan.o \
	src/tk.o \
	src/sym.o \
	src/intermediate.o

DEPS = $(patsubst %.o,%.deps,$(OBJS))

CC = gcc
CC_FLAGS += -O0 -I./src/ -fsanitize=undefined -Wno-pedantic -std=c11 -pg -rdynamic

LNK = gcc
LNK_FLAGS += -o $(OUT) -rdynamic -pg
LNK_LIBS += -lubsan

all: $(OUT)

run: all
	./$(OUT) test.nyx

$(OUT):	$(OBJS)
	$(LNK) $(LNK_FLAGS) $(OBJS) $(LNK_LIBS)

%.o: %.c Makefile
	$(CC) -MM -MT $@ -MF $(patsubst %.o,%.deps,$@) $< $(CC_FLAGS)
	$(CC) -c $< -o $@ $(CC_FLAGS)

-include $(DEPS)

sync:
	git ls-files | sed -e 's/^/..\//' > IDE/onyxc.files

clean:
	rm $(OBJS) $(DEPS)

.PHONY: all clean run sync
