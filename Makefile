.PHONY: run build test clean install

# Default target runs build + tests
all: build test

# Optionally update index and build manually
install:
	cabal update
	cabal build

# Build (no update by default)
build:
	cabal build

# Run tests directly (no install dependency)
test:
	cabal test

# "Run" for a library: run tests as a simple demo
run: build
	@echo "Running grasslands library demo via tests..."
	cabal test

# Clean build artifacts
clean:
	cabal clean
