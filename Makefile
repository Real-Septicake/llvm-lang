C_FILES = $(wildcard *.cpp *.hpp)
BUILD_DIR := build
DEBUG ?= false

SHOW_EXEC ?= false

COMPILER_OBJS := scanner.o parser.o token.o error.o expr.o stmt.o value.o ast_printer.o compiler.o arg_parse.o
COMPILER_MAIN := compiler_main.o
COMPILER_OUT := compiler.out

CPP_STD := -std=c++20

C_FLAGS := -fexceptions -pipe

INCLUDES := -Itermcolor/include

ifeq ($(DEBUG), true)
DEBUG_FLAGS := -ggdb
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

.PHONY: all
all: build-dir compiler
	@echo "DEBUG is set to $(DEBUG)"

.PHONY: loc
loc:
	cloc --exclude-dir=termcolor .

.PHONY: link-file
link-file:
	@g++ -O0 main.cpp out.o

.PHONY: format
format: $(C_FILES)
	@for file in $?; do \
		clang-format -i $$file && echo "formatted $$file"; \
	done

build-dir:
	$(SILENCE)test -d $(BUILD_DIR) || mkdir $(BUILD_DIR)
	
%.o : %.cpp
	@echo "Compiling $@..."
	$(SILENCE)g++ `llvm-config-18 --cxxflags` $(C_FLAGS) -c $(DEBUG_FLAGS) $(OPT_FLAGS) $(INCLUDES) $< -o $(BUILD_DIR)/$@
	
.PHONY: compiler
compiler: $(COMPILER_OBJS) $(COMPILER_MAIN)
	@echo "Building compiler..."
	$(SILENCE)d=$$(date +%s) \
	; g++ $(DEBUG_FLAGS) $(OPT_FLAGS) $(addprefix $(BUILD_DIR)/, $(COMPILER_MAIN) $(COMPILER_OBJS)) $(patsubst %.o,%.hpp, $(COMPILER_OBJS)) \
	`llvm-config-18 --cxxflags --system-libs --libs core native` $(C_FLAGS) -o $(COMPILER_OUT) \
	&& echo "Compiler build took $$(($$(date +%s)-d)) seconds."

gen-ast:
	python3 gen_ast.py
	@make format

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)
	rm -f $(COMPILER_OUT)