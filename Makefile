# Compiler
CC = gcc
# Compiler flags
CFLAGS = -Wall -Wextra -std=c17 -I$(INC_DIR)
# Debug flags
DEBUG_FLAGS = -g
# Source directory
SRC_DIR = ./src
# Include directory
INC_DIR = ./include
# Output directory
OUT_DIR = ./build
# Source files
SOURCES = $(wildcard $(SRC_DIR)/*.c)
# Object files
OBJECTS = $(SOURCES:$(SRC_DIR)/%.c=$(OUT_DIR)/%.o)
# Executable name
EXECUTABLE = clox

# Default target
all: $(EXECUTABLE)

# Debug target
debug: CFLAGS += $(DEBUG_FLAGS)
debug: $(EXECUTABLE)_debug

# Compile
$(OUT_DIR)/%.o: $(SRC_DIR)/%.c | $(OUT_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Link
$(EXECUTABLE): $(OBJECTS)
	$(CC) $(CFLAGS) $^ -o $@

# Link debug version
$(EXECUTABLE)_debug: CFLAGS += $(DEBUG_FLAGS)
$(EXECUTABLE)_debug: $(OBJECTS)
	$(CC) $(CFLAGS) $^ -o $@

# Create build directory
$(OUT_DIR):
	mkdir -p $@

# Clean
clean:
	rm -rf $(OUT_DIR) $(EXECUTABLE) $(EXECUTABLE)_debug

# Phony targets
.PHONY: all debug clean
