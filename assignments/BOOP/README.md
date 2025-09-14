# BOOP Assignment: Introduction to Computer Science

## Overview

This assignment consists of four problems that will help you develop skills in algorithmic thinking, formal specification, and program verification using the BOOP (Blueprint-Operational-OCaml-Proof) methodology.

## BOOP Extension Setup

Before starting this assignment, you must install the BOOP extension for Visual Studio Code:

1. **Install the Extension**: Go to [BOOP for CS Pedagogy](https://marketplace.visualstudio.com/items?itemName=vaanigoenka.boop-for-cs-pedagogy) and install it in VS Code.

2. **Verify Installation**: After installation, you should see BOOP-specific syntax highlighting and formatting when working with `.ics` files.

## Assignment Structure

The assignment contains four problems:

1. **Problem 1: Division as Repeated Subtraction** - Implement division using natural numbers
2. **Problem 2: List Reversal** - Reverse a list using recursive algorithms
3. **Problem 3: Egyptian Fractions** - Express fractions as sums of unit fractions
4. **Problem 4: Stalin Sort** - Implement a sorting algorithm that removes out-of-order elements

## BOOP Methodology

Each problem follows the BOOP format with four essential components:

### 1. Blueprint Section
- **Purpose**: Define the formal specification of your algorithm
- **Content**: 
  - `requires`: Preconditions that must be satisfied for the function to work correctly
  - `ensures`: Postconditions that describe what the function guarantees to return
- **Example**:
  ```
  requires: n >= 0, m > 0
  ensures: returns (q, r) such that ...
  ```

### 2. Operational Steps Section
- **Purpose**: Describe the algorithmic approach in plain English
- **Content**: Step-by-step description of how your algorithm works
- **Guidelines**:
  - Use clear, concise language
  - Focus on the algorithmic strategy, not implementation details
  - Include base cases and recursive cases
  - Mention any special handling for edge cases

### 3. OCaml Code Section
- **Purpose**: Provide the actual implementation
- **Content**: Complete, working OCaml code
- **Requirements**:
  - Code must be syntactically correct
  - Include proper type annotations
  - Handle all edge cases mentioned in the blueprint
  - Use appropriate OCaml constructs (pattern matching, recursion, etc.)

### 4. Proof Section
- **Purpose**: Demonstrate correctness of your algorithm
- **Content**: Rigorous proof of correctness
- **Structure**:
  - **Induction**: Use the principle of induction to show that is if the input satisfies the requires clause (preconditions), then the output must satisfy the ensures clause (postconditions).
  - **Termination**: Argue that the algorithm always terminates

## Problem-Specific Guidelines

### Problem 1: Division as Repeated Subtraction
- Work with natural numbers (Zero, Succ constructors)
- Handle division by zero appropriately
- Consider both quotient and remainder
- Reference the division algorithms discussed in Lecture 6

### Problem 2: List Reversal
- Use OCaml's built-in list type
- Implement both naive and efficient versions if possible
- Consider space and time complexity
- Handle empty lists and single-element lists

### Problem 3: Egyptian Fractions
- Research the greedy algorithm for Egyptian fractions
- Handle proper fractions (numerator < denominator)
- Ensure all fractions in the result are unit fractions (numerator = 1)
- Consider termination and correctness of the greedy approach

### Problem 4: Stalin Sort
- Implement the "sorting" algorithm that removes elements that break the order
- Return the longest possible sorted subsequence
- Consider the relationship to longest increasing subsequence
- Handle edge cases like empty lists and already-sorted lists

## Submission Guidelines

1. **File Format**: Submit your solutions as `.ics` files using the provided templates
2. **Naming**: Keep the original filenames (`problem-01.ics`, `problem-02.ics`, etc.)
3. **Completeness**: Fill in all four sections (Blueprint, Operational Steps, OCaml Code, Proof) for each problem
4. **Header Information**: Update the header with your name, date, and collaborators (if any)

## Academic Integrity

- You may discuss general algorithmic approaches with classmates
- You must write your own code and proofs
- Cite any external resources you consult
- List all collaborators in the header section