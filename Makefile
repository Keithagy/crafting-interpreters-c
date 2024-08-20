# Compiler
CC = gcc

# Compiler flags
CFLAGS = -Wall -Wextra -std=c17

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
all: $(OUT_DIR) $(EXECUTABLE)

# Create build directory
$(OUT_DIR):
	mkdir -p $(OUT_DIR)

# Compile
$(OUT_DIR)/%.o: $(SRC_DIR)/%.c
	$(CC) $(CFLAGS) -I$(INC_DIR) -c $< -o $@

# Link
$(EXECUTABLE): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@

# Clean
clean:
	rm -rf $(OUT_DIR) $(EXECUTABLE)

# Phony targets
.PHONY: all clean
