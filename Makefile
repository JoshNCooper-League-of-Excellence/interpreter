PRJ_NAME := bindings
COMPILER := clang
COMPILER_FLAGS := -std=c23 -Iinclude -g -Wall -Wextra
LD_FLAGS := -lffi
SRC_EXT_PAT := src/%.c
OBJ_DIR := obj
REL_OBJ_DIR := obj/release
DBG_OBJ_DIR := obj/debug
REL_BIN_DIR := bin/release
DBG_BIN_DIR := bin/debug
BIN_DIR := bin
SRCS := $(wildcard src/*.c)

REL_OBJS := $(patsubst $(SRC_EXT_PAT), $(REL_OBJ_DIR)/%.o, $(SRCS))
DBG_OBJS := $(patsubst $(SRC_EXT_PAT), $(DBG_OBJ_DIR)/%.o, $(SRCS))

all: directories $(PRJ_NAME)

directories:
	mkdir -p $(REL_OBJ_DIR) $(DBG_OBJ_DIR) $(DBG_BIN_DIR) $(REL_BIN_DIR)

$(PRJ_NAME): $(DBG_OBJS)
	$(COMPILER) $(COMPILER_FLAGS) -g -o $(DBG_BIN_DIR)/$(PRJ_NAME) $^ $(LD_FLAGS)

release: $(REL_OBJS)
	$(COMPILER) $(COMPILER_FLAGS) -O3 -o $(REL_BIN_DIR)/$(PRJ_NAME) $^ $(LD_FLAGS)

$(REL_OBJ_DIR)/%.o: $(SRC_EXT_PAT)
	$(COMPILER) $(COMPILER_FLAGS) -c $< -o $@

$(DBG_OBJ_DIR)/%.o: $(SRC_EXT_PAT)
	$(COMPILER) $(COMPILER_FLAGS) -c $< -o $@

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

run: all $(PRJ_NAME)
	./$(DBG_BIN_DIR)/$(PRJ_NAME)

run-release: directories release
	./$(REL_BIN_DIR)/$(PRJ_NAME)