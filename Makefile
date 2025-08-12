.PHONY: build run test clean

# Compiler and flags
GHC = ghc
GHC_FLAGS = -Wall -O2
TEST_FILE = test/Tests.hs

# Build the project
build:
	$(GHC) $(GHC_FLAGS) -ilib -itest -o grass lib/Main.hs

# Run the program
run: build
	./grass

# Run tests
test:
	runghc -ilib -itest $(TEST_FILE)

# Clean build artifacts
clean:
	rm -f grass
	rm -f *.hi *.o
	rm -f lib/*.hi lib/*.o
	rm -f test/*.hi test/*.o

# Install dependencies (only GHC and HSpec are needed)
setup:
	ghc --version
	ghc-pkg list | grep hspec || echo "HSpec not found. Run: cabal install hspec"
	cabal update
	cabal install --dependencies-only
