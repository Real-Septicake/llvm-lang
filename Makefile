C_FILES = $(wildcard *.cpp *.hpp)
BUILD_DIR := build
DEBUG ?= false

SHOW_EXEC ?= false

COMPILER_OBJS := scanner.o parser.o token.o error.o expr.o stmt.o value.o ast_printer.o
COMPILER_MAIN := test.o
COMPILER_OUT := compiler.out

CPP_STD := -std=c++20

INCLUDES := -Itermcolor/include

ifeq ($(DEBUG), true)
DEBUG_FLAGS := -ggdb -DDEBUG
OPT_FLAGS := -O0
else
DEBUG_FLAGS :=
OPT_FLAGS := -Os
endif

ifeq ($(SHOW_EXEC), true)
SILENCE :=
else
SILENCE := @
endif

format: $(C_FILES)
	@for file in $?; do \
		clang-format -i $$file && echo "formatted $$file"; \
	done

build-dir:
	$(SILENCE)test -d $(BUILD_DIR) || mkdir $(BUILD_DIR)
	
%.o : %.cpp
	$(SILENCE)g++ $(CPP_STD) -c $(DEBUG_FLAGS) $(OPT_FLAGS) $(INCLUDES) $< -o $(BUILD_DIR)/$@
	
compiler: $(COMPILER_OBJS) $(COMPILER_MAIN)
	@echo "Building compiler..."
	$(SILENCE)d=$$(date +%s) \
	; g++ $(CPP_STD) $(DEBUG_FLAGS) $(OPT_FLAGS) $(addprefix $(BUILD_DIR)/, $(COMPILER_MAIN) $(COMPILER_OBJS)) $(patsubst %.o,%.hpp, $(COMPILER_OBJS)) -o $(COMPILER_OUT) \
	&& echo "Compiler build took $$(($$(date +%s)-d)) seconds."

gen-ast:
	python gen_ast.py
	@make format

clean:
	rm -rf $(BUILD_DIR)
	rm -f $(COMPILER_OUT)

all: build-dir compiler
	@echo "DEBUG is set to $(DEBUG)"