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
	@rm -rf $(BIN_DIR)

test:
	@cd tests; \
	time for file in ./*; do \
		if [ -f "$$file" ]; then \
			canon=$$(readlink -f "$$file"); \
			../bin/compiler "$$canon"; \
			if [ $$? -eq 0 ]; then \
				echo -e "\033[1;32m$$canon passed\033[0m"; \
			else \
				echo -e "\033[1;31m$$canon failed\033[0m"; \
			fi; \
		fi; \
	done


run: all
	./$(BIN)