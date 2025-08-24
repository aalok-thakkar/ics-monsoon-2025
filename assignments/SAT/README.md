# CDCL SAT Solver Implementation

## The SAT Problem Challenge

The Boolean Satisfiability Problem (SAT) is one of the most fundamental problems in computer science. Given a Boolean formula in Conjunctive Normal Form (CNF), the task is to determine whether there exists an assignment of truth values to variables that makes the entire formula true.

SAT was the first problem proven to be NP-complete by Stephen Cook in 1971, making it a cornerstone of computational complexity theory. Modern SAT solvers use sophisticated algorithms like Conflict-Driven Clause Learning (CDCL) to solve instances with thousands of variables efficiently.

## Your Mission: Implement a CDCL SAT Solver

Your task is to convert a complete CDCL SAT solver from Python to OCaml. This involves understanding the algorithm, data structures, and implementation details, then recreating the entire system in a functional programming language.

## Implementation Requirements

### Input and Output Format

Your OCaml implementation must maintain exact compatibility with the Python version:

**Input**: DIMACS CNF format (more details below)

**Output**: 
- For SAT instances: "SAT" followed by the satisfying assignment
- For UNSAT instances: "UNSAT"

### Core Algorithm Components

1. **Unit Propagation**: Using watched literals for efficiency
2. **Conflict Analysis**: Learning new clauses from conflicts
3. **Backtracking**: Non-chronological backtracking based on learned clauses
4. **Variable Selection**: Deterministic selection (first unassigned variable)
5. **Value Selection**: Deterministic selection (always True first)

### Key Data Structures

- **Clauses**: Lists of literal tuples (variable, is_negated)
- **Assignments**: Mapping variables to (value, antecedent, decision_level)
- **Watched Literals**: For efficient unit propagation
- **Decision Stack**: For tracking decision levels

## Reference Implementation

The Python implementation provides a complete working example:

- **`src/sat_solver.py`**: Core CDCL algorithm
- **`src/parser.py`**: DIMACS format parsing and solution writing
- **`main.py`**: Main entry point and program flow

## DIMACS CNF Format

The DIMACS CNF format is the standard format for representing Boolean formulas in Conjunctive Normal Form.

### Format Specification

- **Comments**: Lines starting with 'c' are comments
- **Problem line**: `p cnf <num_variables> <num_clauses>`
- **Clauses**: Each clause is a space-separated list of integers ending with 0
  - Positive integers represent positive literals (e.g., 1 = x₁)
  - Negative integers represent negative literals (e.g., -1 = ¬x₁)
  - 0 marks the end of each clause

### Example

```
c Example CNF formula: (x1 OR x2) AND (NOT x1 OR x3) AND (NOT x2 OR NOT x3)
c Expected: SAT
p cnf 3 3
1 2 0
-1 3 0
-2 -3 0
```

This represents the formula: (x₁ ∨ x₂) ∧ (¬x₁ ∨ x₃) ∧ (¬x₂ ∨ ¬x₃)

## Project Structure

```
SAT/
├── src/
│   ├── parser.py      # DIMACS parsing and solution writing
│   ├── sat_solver.py  # Core CDCL algorithm implementation
│   └── main.py        # Main entry point
├── tests/
│   ├── input/         # Test input files (test_1.cnf to test_6.cnf)
│   └── output/        # Expected output files
└── README.md
```

## Running the Python Version

```bash
python3 main.py <input_file> <output_file>
```

Example:
```bash
python3 main.py tests/input/test_1.cnf tests/output/test_1_result.txt
```

## Your Challenge

1. **Understand the CDCL algorithm** by studying the Python implementation
2. **Convert the entire system to OCaml** while maintaining exact functionality
3. **Test your implementation** against all provided test cases
4. **Ensure compatibility** with the original input/output format

Remember: The goal is not just to create a working SAT solver, but to understand and implement the sophisticated algorithms that make modern SAT solving possible.

Good luck with your implementation!

