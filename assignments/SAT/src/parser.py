#!/usr/bin/env python3

def parse_dimacs(filename):
    """
    Requires: filename is a valid path to a DIMACS CNF file
    Ensures: returns (num_vars, clauses) where num_vars is the number of variables and clauses is a list of clause lists
    """
    clauses = []
    num_vars = 0
    
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            if line.startswith('c') or not line:
                continue
            elif line.startswith('p'):
                parts = line.split()
                if len(parts) >= 4 and parts[1] == 'cnf':
                    num_vars = int(parts[2])
            else:
                literals = []
                parts = line.split()
                for part in parts:
                    if part == '0':
                        break
                    val = int(part)
                    if val == 0:
                        continue
                    literals.append(val)
                if literals:
                    clauses.append(literals)
    
    return num_vars, clauses


def write_solution(filename, assignment):
    """
    Requires: filename is a valid path, assignment is None or a dict mapping variables to booleans
    Ensures: writes SAT/UNSAT result to filename in DIMACS format
    """
    with open(filename, 'w') as f:
        if assignment is None:
            f.write("UNSAT\n")
        else:
            f.write("SAT\n")
            for var in sorted(assignment.keys()):
                val = assignment[var]
                literal_val = var if val else -var
                f.write(f"{literal_val} ")
            f.write("0\n")
