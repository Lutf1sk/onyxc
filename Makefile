
OUT = onyxc

OBJS = \
	type.o \
	parse.o \
	ast.o \
	main.o \
	filehl.o \
	err.o \
	scan.o \
	tk.o \
	sym.o

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

clean:
	rm $(OBJS) $(DEPS)

.PHONY: all clean run
