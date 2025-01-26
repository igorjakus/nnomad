# Nomad

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![OCaml](https://img.shields.io/badge/OCaml-4.14.2%2B-blue.svg)
![Dune](https://img.shields.io/badge/Dune-3.17.0-blue.svg)

Nomad is a comprehensive OCaml library designed for symbolic expression manipulation and optimization. It provides functionalities for evaluating expressions, computing derivatives, and performing various optimization algorithms such as Gradient Descent, Newton-Raphson, and the Bisection method. This project serves as a practical assignment for the Functional Programming course at the University of Wrocław.

## Table of Contents

- [Features](#features)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage](#usage)
- [Testing](#testing)
- [Project Structure](#project-structure)
- [Contributing](#contributing)
- [License](#license)
- [Acknowledgments](#acknowledgments)

## Features

- **Symbolic Expression Handling**
  - Define and manipulate mathematical expressions symbolically.
  - Simplify expressions using algebraic rules.
  - Convert expressions to their string representations.

- **Evaluation and Environment Management**
  - Evaluate expressions given a set of variable assignments.
  - Update and manage environments for variable values.

- **Derivatives and Gradients**
  - Compute first and higher-order derivatives of expressions.
  - Calculate gradients for multivariable functions.

- **Optimization Algorithms**
  - **Gradient Descent**: Optimize expressions by iteratively moving towards the minimum.
  - **Newton-Raphson**: Solve equations using the Newton-Raphson method.
  - **Bisection Method**: Find roots of continuous functions.

- **Comprehensive Testing**
  - Robust test suite to ensure correctness of all functionalities.

## Requirements

- **OCaml**: Version 4.14.2 or newer
- **Dune**: Version 3.17.0 or newer

## Installation

1. **Install Dune**
```bash
opam install dune.3.17.0
```

2. **Clone the Repository**
```bash
git clone https://github.com/igorjakus/nomad.git
cd nomad
```

3. **Build project**
```bash
dune build
```

## Usage

### Evaluating Expressions

```ocaml
open Nomad.Expr
open Nomad.Eval
let x, y = Var "x", Var "y"

let env = [("x", 2.0); ("y", 3.0)]
let expression = x +: y
let result = eval env expression
(* result = 5.0 *)
```

### Simplifying Expressions

```ocaml
open Nomad.Expr
open Nomad.Simplify

let expr = x +: Float 0.
let simplified = simplify expr
(* simplified = x *)
```

### Computing Derivatives

```ocaml
open Nomad.Derivatives

let expr = x *: x
let derivative = derivative "x" expr
(* derivative = Float 2. *: x *)
```

### Performing Gradient Descent

```ocaml
open Nomad.Gradient_descent

let expr = x *: x +: y *: y
let initial_env = [("x", 1.0); ("y", 1.0)]
let learning_rate = 0.1
let result = gradient_descent ~expr ~env:initial_env ~learning_rate ~max_iter:100
```

## Testing

Nomad includes a comprehensive test suite to validate its functionalities. To run the tests, execute:

```bash
dune runtest
```

This will execute all test cases defined in the test.ml file, ensuring that each component behaves as expected.

## Contributing

Contributions are welcome! Please submit issues and pull requests for any enhancements or bug fixes.

1. Fork the repository.
2. Create a new branch: git checkout -b feature/YourFeature
3. Commit your changes: git commit -m 'Add some feature'
4. Push to the branch: git push origin feature/YourFeature
5. Submit a pull request.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Developed as part of the Functional Programming course at the University of Wrocław.
- Inspired by various symbolic mathematics and optimization libraries.