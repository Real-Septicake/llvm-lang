C_FILES = $(wildcard *.cpp *.hpp)
BUILD_DIR := build
DEBUG ?= false

SHOW_EXEC ?= false

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
	
%.o : %.cc
	$(SILENCE)g++ $(CPP_STD) -c $(DEBUG_FLAGS) $(OPT_FLAGS) $(INCLUDES) $< -o $(BUILD_DIR)/$@
	
gen-ast:
	python gen_ast.py
	@make format