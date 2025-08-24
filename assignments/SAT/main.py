#!/usr/bin/env python3

import sys
import time
from src.parser import parse_dimacs, write_solution
from src.sat_solver import solve_sat


def main():
    if len(sys.argv) != 3:
        print("Usage: python main.py <input_file> <output_file>")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    
    try:
        num_vars, clauses = parse_dimacs(input_file)
        
        print(f"Solving SAT instance with {num_vars} variables and {len(clauses)} clauses...")
        start_time = time.time()
        
        assignment = solve_sat(num_vars, clauses)
        
        end_time = time.time()
        print(f"Solved in {end_time - start_time:.4f} seconds")
        
        write_solution(output_file, assignment)
        
        if assignment:
            print("SAT - Solution found!")
        else:
            print("UNSAT - No solution exists")
            
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
