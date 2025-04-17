# Fixed-Point Parsing Framework and Personal Parsers

This repository contains a fixed-point parsing framework and a collection of personal parsers built using it. The framework is designed to support left-recursive and ambiguous parsing through memoization, making it suitable for complex grammars. The repository also includes example parsers and test cases to demonstrate its capabilities.

## Features

- **Fixed-Point Parsing Framework**: A modular and extensible framework for building parsers with support for memoization and recursive grammars.
- **Example Parsers**:
  - **Expression Parser**: Parses mathematical expressions with support for addition, subtraction, multiplication, and division.
  - **Tego Parser**: Parses a custom language called [Tego](https://github.com/pixilcode/tego-lang), which includes constructs like functions, conditionals, and pattern matching.
- **Test Suite**: Comprehensive test cases for the framework and parsers using OUnit2.

## Getting Started

### Prerequisites

- OCaml (>= 4.12.0)
- Dune (>= 3.17)
- OPAM for dependency management

### Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/pixilcode/fpp_personal_parsers.git
   cd fpp_personal_parsers
   ```

2. Install dependencies:
   ```bash
   opam install . --deps-only
   ```

3. Build the project:
   ```bash
   dune build
   ```

### Running the CLI

The repository includes a command-line interface for parsing input strings using the available parsers.

```bash
dune exec parse -- <parser> <input>
```

- `<parser>`: The parser to use (`expr` for the expression parser, `tego` for the Tego parser).
- `<input>`: The input string to parse.

Example:
```bash
dune exec parse expr "1+2*3"
```

### Running Tests

To run the test suite:
```bash
dune test
```

## Example Usage

### Expression Parser

The expression parser supports basic arithmetic operations:
```bash
dune exec parse expr "3+5*(2-1)"
```

### Tego Parser

The Tego parser can parse and analyze programs written in the Tego language:
```bash
dune exec parse tego "def main = addOne 1"
```

## Development Notes

- The framework uses memoization to handle recursive grammars efficiently.
- The notes.md file contains insights and challenges encountered during development.

## License

This project is licensed under the MIT License. See the `LICENSE` file for details.
