PRJ_NAME := compiler
CC := clang
CFLAGS := -std=c23 -Iinclude -g -Wall -Wextra
LD_FLAGS := -lffi
BIN_DIR := bin
BIN := $(BIN_DIR)/$(PRJ_NAME)
SRCS := $(wildcard src/*.c)

all: $(BIN)

directories:
	mkdir -p $(BIN_DIR)

$(BIN): directories
	$(CC) $(CFLAGS) -o $(BIN) $(SRCS) $(LD_FLAGS)

clean:
	rm -rf $(BIN_DIR)

test:
	@./run_tests.sh

run: all
	./$(BIN)