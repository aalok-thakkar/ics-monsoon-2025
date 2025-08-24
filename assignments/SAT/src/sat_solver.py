import sys
from typing import List, Set, Tuple, Optional, Iterator


def solve_sat(num_vars, clauses):
    """
    Requires: num_vars > 0, clauses is a list of clause lists where each clause contains integers
    Ensures: returns assignment dict if SAT, None if UNSAT
    """
    # Convert clauses to internal format: list of lists of (var, neg) tuples
    formula_clauses = []
    variables = set()
    
    for clause in clauses:
        clause_literals = []
        for lit in clause:
            var = abs(lit)
            neg = lit < 0
            clause_literals.append((var, neg))
            variables.add(var)
        formula_clauses.append(clause_literals)
    
    # Initialize assignments: var -> (value, antecedent, dl)
    assignments = {}  # var -> (value, antecedent, dl)
    current_dl = 0
    
    # Initial unit propagation
    reason, conflict_clause = unit_propagation(formula_clauses, assignments, current_dl)
    if reason == 'conflict':
        return None

    while len(assignments) < len(variables):
        var, val = pick_branching_variable(variables, assignments)
        current_dl += 1
        assignments[var] = (val, None, current_dl)
        
        while True:
            reason, conflict_clause = unit_propagation(formula_clauses, assignments, current_dl)
            if reason != 'conflict':
                break
                
            b, learnt_clause = conflict_analysis(conflict_clause, assignments, current_dl)
            if b < 0:
                return None
            
            formula_clauses.append(learnt_clause)
            backtrack(assignments, b)
            current_dl = b

    # Convert back to required format
    result = {}
    for var, (value, _, _) in assignments.items():
        result[var] = value
    
    return result


def pick_branching_variable(variables, assignments):
    """
    Requires: variables is a set of integers, assignments is a dict
    Ensures: returns (variable, value) for next branching decision
    """
    unassigned_vars = [var for var in variables if var not in assignments]
    var = unassigned_vars[0]  # Fixed choice: always pick first unassigned variable
    val = True  # Fixed choice: always prefer True
    return (var, val)


def backtrack(assignments, b):
    """
    Requires: assignments is a dict, b is a non-negative integer
    Ensures: backtracks to decision level b, removing all assignments with dl > b
    """
    to_remove = []
    for var, (_, _, dl) in assignments.items():
        if dl > b:
            to_remove.append(var)
            
    for var in to_remove:
        del assignments[var]


def clause_status(clause, assignments):
    """
    Requires: clause is a list of (var, neg) tuples, assignments is a dict
    Ensures: returns status of clause: 'satisfied', 'unsatisfied', 'unit', or 'unresolved'
    """
    values = []
    for var, neg in clause:
        if var not in assignments:
            values.append(None)
        else:
            val, _, _ = assignments[var]
            if neg:
                values.append(not val)
            else:
                values.append(val)

    if True in values:
        return 'satisfied'
    elif values.count(False) == len(values):
        return 'unsatisfied'
    elif values.count(False) == len(values) - 1:
        return 'unit'
    else:
        return 'unresolved'


def unit_propagation(formula_clauses, assignments, current_dl):
    """
    Requires: formula_clauses is a list of clause lists, assignments is a dict, current_dl is an integer
    Ensures: performs unit propagation, returns ('conflict', clause) if conflict found, ('unresolved', None) otherwise
    """
    finish = False
    while not finish:
        finish = True
        for clause in formula_clauses:
            status = clause_status(clause, assignments)
            if status == 'unresolved' or status == 'satisfied':
                continue
            elif status == 'unit':
                # Find the unassigned literal
                for var, neg in clause:
                    if var not in assignments:
                        val = not neg
                        assignments[var] = (val, clause, current_dl)
                        finish = False
                        break
            else:
                return ('conflict', clause)

    return ('unresolved', None)


def resolve(a, b, x):
    """
    Requires: a and b are lists of (var, neg) tuples, x is a positive integer
    Ensures: returns resolution of clauses a and b on variable x
    """
    result = set()
    
    # Add all literals from both clauses
    for var, neg in a + b:
        if var != x:  # Exclude the resolution variable
            result.add((var, neg))
    
    return list(result)


def conflict_analysis(clause, assignments, current_dl):
    """
    Requires: clause is a list of (var, neg) tuples, assignments is a dict, current_dl is an integer
    Ensures: returns (backtrack_level, learnt_clause) from conflict analysis
    """
    if current_dl == 0:
        return (-1, None)
 
    # Find literals with current decision level
    literals = []
    for var, neg in clause:
        if var in assignments:
            _, _, dl = assignments[var]
            if dl == current_dl:
                literals.append((var, neg))
    
    while len(literals) != 1:
        # Find implied literals (those with antecedents)
        implied = []
        for var, neg in literals:
            if var in assignments:
                _, antecedent, _ = assignments[var]
                if antecedent is not None:
                    implied.append((var, neg))
        
        if not implied:
            break
            
        # Select any implied literal
        literal = implied[0]
        var, neg = literal
        antecedent = assignments[var][1]
        
        # Resolve
        clause = resolve(clause, antecedent, var)
        
        # Update literals with current decision level
        literals = []
        for v, n in clause:
            if v in assignments:
                _, _, dl = assignments[v]
                if dl == current_dl:
                    literals.append((v, n))

    # Compute backtrack level
    decision_levels = set()
    for var, neg in clause:
        if var in assignments:
            _, _, dl = assignments[var]
            decision_levels.add(dl)
    
    decision_levels = sorted(decision_levels)
    if len(decision_levels) <= 1:
        return 0, clause
    else:
        return decision_levels[-2], clause
