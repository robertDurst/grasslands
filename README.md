# Haskell Hello World

A minimal Haskell package demonstrating a simple "Hello, World!" program with tests and CI setup.

## Prerequisites

- [GHC](https://www.haskell.org/ghc/) (The Glasgow Haskell Compiler)
- [Cabal](https://www.haskell.org/cabal/) (Haskell's build tool)

## Getting Started

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd haskell-hello-world
   ```

2. Install dependencies:
   ```bash
   make setup
   ```

## Building the Project

```bash
make build
```

## Running the Program

```bash
make run
```

## Running Tests

```bash
make test
```

## Cleaning Up

To clean build artifacts:

```bash
make clean
```

## CI/CD

This project uses GitHub Actions for continuous integration. The workflow runs on every push to the main/master branch and on pull requests, executing the test suite.
