(* Can computers solve all problems? *)

(* Type Definitions *)

type program = string
type input = string
type problem = input -> bool












(* We will work with only nat as input for now *)

type nat = Zero | Succ of nat
type problem_nat = nat -> bool








(* The cardinality of programs = the cardinality of natural numbers *)






(* Each problem is a subset of natural numbers. For sake of contradiction, let us say we can enumerate the subsets of natural numbers. 

S0. {0: false, 1: true, 2: false, 3: true, 4: false, 5: true, ...}
S1. {0: false, 1: false, 2: true, 3: true, 4: false, 5: true, ...}
S2. {0: false, 1: true, 2: true, 3: true, 4: true, 5: true, ...}
...






Then we can consider the 
Paradox set --> 
    {0: mark the opposite of what I had in the zeroth set, 
     1: mark the opposite of what I had in the first set, 
     2: mark the opposite of what I had in the second set,
     ... }

Paradox set --> 
    P = {0: true, 1: true, 2: false, 3: ... , }

    P = {i: neg Si[i] for all i in nat}

    Then, paradox set != Si for all i, as it differs at the ith index.
*)






(* Is there a concrete unsolvable problem? *)





(* Let us look at Liar's Paradox! *)

(* This statement is false. *)

(* Can we automate program correctness? *)

(* Can we create a program that takes in another program (like a higher order function), and checks if that input program is correct.   
  IF so, ask the program to check for its own correctness. *)


(* Given program p and specific input i, does p halt on i? *)












(* Setup *)

(* 
   For the sake of contradiction, we assume there exists a function
   that can decide the halting problem for any program/input pair.   
*)



module type HaltingDecider = sig
  val decides_halting : program -> input -> bool
  (* 
     Specification: 
     - returns true if program halts on input 
     - returns false if program does not halt on input
     - must work for ALL valid program/input pairs
  *)
end



(* 3. Proof by Contradiction *)

(* 
   Assume (for contradiction) there exists a halting decider 'halting'.
   'halting' takes a program p and input i, and correctly returns:
   - true if p halts on i
   - false if p runs forever on i
*)
let halting (p : program) (i : input) : bool = 
  (* Hypothetical implementation *)
  failwith "This cannot actually exist"

(* 
   Construct the paradoxical program 'paradox':
   - Takes a program p as input
*)
let paradox (p : program) : unit =
  if halting p p then 
    (* - Simulates 'halting' asking whether p halts when given itself as input. If H says p halts on p, loop forever *)
    let rec loop () = loop () in loop ()
  else 
    (* If H says p doesn't halt on p, halt immediately *)
    ()

(* 
   The contradiction arises when we consider 'paradox' run on itself:
   
   Case 1: If 'halting' says 'paradox' halts on 'paradox':
     - Then 'paradox' enters infinite loop (so 'halting' was wrong)
   
   Case 2: If 'halting' says 'paradox' doesn't halt on 'paradox':
     - Then 'paradox' halts immediately (so 'halting' was wrong)
   
   In both cases, 'halting' gives the wrong answer.
*)

(* 
   Therefore, no such halting decider H can exist that works
   for all possible programs and inputs.
*)
