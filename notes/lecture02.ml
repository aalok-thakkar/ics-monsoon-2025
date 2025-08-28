exception Not_implemented of string


(* Lecture 2: Natural Numbers and the Axiom of Induction *)

(* 
    Fun Fact: Insects have ears in unusual places--crickets on legs, 
    grasshoppers on abdomens, moths on thoraxes, and mantises with a single 
    ear on their chest to dodge bats!
*)


















(* In the last class we saw native type int *)

let x : int = 5
let y : int = 2
let z : int = -3
let sum : int = x + y
let product : int = x * y
let quotient : int = x / y
let remainder : int = x mod y

(* Integer division truncates towards zero *)
let div1 : int = 5 / 2     (* 2 *)
let div2 : int = (-5) / 2  (* -2, not -3 *)

(* Remainder (modulus) behaves accordingly *)
let rem1 : int = 5 mod 2    (* 1 *)
let rem2 : int = (-5) mod 2 (* -1, result sign matches dividend *)






let is_odd (n : int) : bool = 
  match (n mod 2) with 
  | 0 -> true
  | _ -> false

let collatz (n : int) : int = 
  match (is_odd n) with 
  | true -> 3 * n + 1
  | false -> n / 2






(* What if we want fractions? *)









(* Floats are written with decimal points or exponents *)
let pi : float = 3.14159
let temp : float = -2.5
let three : float = 3.
let int_three : int = 3

let sum : float = 0.1 +. 0.2
let product : float = 0.1 *. 0.2


let a : float = float_of_int 5 

let b : int = int_of_float 3.7
let c : int = int_of_float (-3.7)

(* If n is an int, 

    then this is true: int_of_float (float_of_int (n)) == n

    If m is a float, 
    
    This may not be true float_of_int (int_of_float (m)) == m


*)

let d : float = 3.0 +. 0.1









(* Can the native types be wrong? *)

let add_int (x: int) (y: int) : int = x + y 

(* What is right? What is wrong? *)

(*  az kufr-o-ze-islam barun sahrayist
    ma ra ba-miyan-e-aan faza saudayist  *)









(* 
    Let Z be the set of all integers. And let 
    (+): Z * Z -> Z be the standard addition function. 
    
    Then the function add_int should implement it. 
    
    That is, if x : int and y : int, let [[x]] and [[y]] denote the 
      corresponding mathematical values in Z. 

    Then add_int is correct if for all x : int and y : int, we require

    [[add_int x y]] = [[x]] + [[y]]
*)


(* Theorem: 

    For all m : int, 
        m + 1 > m.
*)





let m : int = 4611686018427387903
















(* 64-bit signed number *)

let upper_bound : int = max_int
let lower_bound : int = min_int

(*
    In 2014, Psy's Gangnam style vidoes view counter got stuck at 2,147,483,647
    because thats the maximum value a 32 bit integer can store. 
*)










(* 
    Case Study III: Ariane flight V88, 1996
    
    Ariane flight V88 carried the Cluster spacecraft, a constellation of four 
    European Space Agency research satellites. Their code had inadequate 
    protection against integer overflow. 

    meters = radians * 6378137.0 / pi

    The problem was the conversion from radians to meters was done using 
    integer arithmetic, which caused an overflow. This made the rocket to veer 
    off and finally self-destruct. Caused a loss of US$370 million.
*)










(* How do you protect against integer overflow? *)



let safe_add_int (x : int) (y : int) : int =
  if (x > 0) && (y > 0) && (x > max_int - y) then
    raise (Invalid_argument "Integer Overflow")
  else if (x < 0) && (y < 0) && (x < min_int - y) then
    raise (Invalid_argument "Negative Underflow")
  else
    x + y









(* 
    This is like astrophysics. Weird things happen at large values. 
    
    It is also like quantum physics. Weird things also happen at small values! 
*)


(* Theorem 2

 For all a : float, b : float, c: float,
        (a +. b) +. c = a +. (b +. c)
*)






(* Floating point arithmetic: 

    On your command line:

    $ ocaml
    # 0.1 +. 0.1 = 0.2;;

    # 0.1 +. 0.2 = 0.3;;

    # 0.1 +. 0.3 = 0.4;;





    # (0.1 +. 0.2) +. 0.3 = 0.1 +. (0.2 +. 0.3) ;; 

*)












(* 
    Case Study IV: Patriot Missile Failure, 1991
    
    During the Gulf War, a Patriot missile battery in Dhahran, Saudi Arabia, 
    failed to intercept an incoming SCUD missile, resulting in the deaths and 
    injuries of 128 U.S. soldiers. The root cause was a floating point error 
    in the system's time calculation: converting 100 hours into tenths of a 
    second introduced a small but critical error due to the limitations of 
    floating point representation, causing the missile tracking software to 
    miscalculate the SCUD's position.
*)






(* 
    There is a lot of such bad code. And LLMs are trained on it! So they are 
    bound to make errors. 
*)















(* If I were to implement numbers in OCaml, I wouldn't make such mistakes! *)



(* But what is a number? What is a natural number? *)





type mynum = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine










(* Peano Axioms *)


(* 1. 0 is a natural number *)
(*    
    That is at least:
      type nat = Zero
*)

(* 2. For every natural number n, Succ n is a natural number *)
(*  How do we capture this? *)

type nat = Zero | Succ of nat 

(* 3. 0 is not the successor of any natural number 
      There is no n such that Succ n = Zero *)




(* 4. Different natural numbers have different successors
      For all m, n: if Succ m = Succ n then m = n *)



(* 5. Axiom of induction: 
       If a property P holds for 0, that is P(0)
       and if it holds for n then it holds for Succ n, 
            that is P(n) -> P(Succ n)
       then it holds for all natural numbers *)



let five : nat = Succ (Succ (Succ (Succ (Succ (Zero))))) (* Number 5 *)
let four : nat = Succ (Succ (Succ (Succ (Zero))))        (* Number 4 *)
       


(* How do we define addition? *)









let plusZero (n : nat) : nat = n

let plusOne (n : nat) : nat = Succ n

let plusTwo (n : nat) : nat = Succ (Succ n)



let plusThree (n : nat) : nat = Succ (Succ (Succ n))


let plusThree (n : nat) : nat = Succ (plusTwo n)

let plusFour (n : nat) : nat = Succ (plusThree n)
let plusFive (n : nat) : nat = Succ (plusFour n)



(* let plus_succ_k (n : nat) : nat = Succ (plus_k n) *)



       










let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 n) 

(* Sample run: 

    plus 3 2 = plus (Succ (Succ (Succ Zero))) (Succ (Succ Zero))
             = Succ plus (Succ (Succ Zero)) (Succ (Succ Zero))
             = Succ (Succ plus (Succ Zero) (Succ (Succ Zero)))
             = Succ (Succ (Succ (plus Zero (Succ (Succ Zero)))))
             = Succ (Succ (Succ (Succ (Succ Zero))))
            

*)





(* Another way: *)

let rec plus2 (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> (plus k2 (Succ n))

(* Sample run: 

    plus2 3 2 = plus2 2 (Succ 2)

*)





(* Exercise: 


let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 n) 

let rec plus2 (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> (plus k2 (Succ n))

  Prove that plus k n = plus2 k n 

    If k = Zero, then we are returning n in both cases. Hence ...

    Inductive Hypothesis: 
     for all n : nat, assume plus k2 n = plus2 k2 n holds

    Inductive Step: show plus (Succ k2) m = plus2 (Succ k2) m

    LHS = Succ (plus k2 m)
    RHS = plus2 k2 (Succ m)

*)



(* Digression: Functions with multiple inputs *)

(* 
    In OCaml, functions with multiple arguments are actually curried.
    This means that a function like:
    
    let plus (k : nat) (n : nat) : nat = ...
    
    is actually equivalent to:
    
    let plus : nat -> (nat -> nat) = fun k -> fun n -> ...
    
    This is called currying, named after Haskell Curry.
*)

(* 
    Currying Diagram:
    
    Traditional view:     plus : nat × nat -> nat
                          (k, n) |-> k + n
                          (3, 5)  -> 3 + 5
    
    Curried view:         plus : nat -> (nat -> nat)
                          k |-> (n |-> k + n)
                          3  -> (n |-> 3 + n)
                          3  -> (5  -> 3 + 5)
    
    Example with plus 3 5:
    
    Step 1: plus 3
    ┌─────────────────────────────┐    ┌─────────────────────────────┐
    │ plus : nat -> (nat -> nat)  │ -> │ fun n -> 3 + n : nat -> nat │
    └─────────────────────────────┘    └─────────────────────────────┘
    
    Step 2: (plus 3) 5
    ┌─────────────────────────────┐    ┌─────────────────────────────┐
    │ fun n -> 3 + n : nat -> nat │ -> │ 3 + 5 = 8 : nat             │
    └─────────────────────────────┘    └─────────────────────────────┘
*)

(* 
    Partial Application:
    
    We can use currying to create specialized functions:
    
    let plus3 : nat -> nat = plus (Succ (Succ (Succ Zero)))
    
    This creates a function that adds 3 to any natural number.
    
    Diagram:
    
    ┌───────────────────────────────────────┐
    │ plus : nat -> (nat -> nat)            │
    │ 3 |-> fun n -> 3 + n : nat -> nat     │
    └───────────────────────────────────────┘
                │
                v
    ┌───────────────────────────────────────┐
    │ plus3 : nat -> nat                    │
    │ n |-> 3 + n                           │
    └───────────────────────────────────────┘
*)

(* What is the type of plus? *)


(* Because of currying, the type of plus is:

   plus : nat -> nat -> nat

   This is equivalent to:
   plus : nat -> (nat -> nat)

   The arrow operator (->) associates to the right, so these two types
   mean exactly the same thing. 
   
*)



(* Is this addition necessarily correct? *)

(*
  Two ways to establish correctness: 
   a. Axiomatic Semantics
   b. Denotational Semantics 
*)


(* Let us prove: 



let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 n) 

    1. plus Zero n = n
    2. plus n Zero = n

    Part 1. 

      plus Zero n
    = n                (hence proved)

    Part 2. 

    To prove: plus n Zero  = n

    Base Case, n = Zero. 

    plus Zero Zero = Zero (pattern matching, line 1)

    Inductive hypothesis: 
    For all n, plus n Zero  = n

    Inductive Step: 

      plus (Succ n) Zero
    = Succ (plus n Zero)       (pattern matching, line 2)
    = Succ (n)                 (using Inductive Hypothesis)

    Hence proved.

    *)



(* 
    Exercise: Convert from nat to int
    
    Write a function nat_to_int : nat -> int that converts our inductive
    natural numbers to OCaml integers.
    
    Examples:
    - nat_to_int Zero = 0
    - nat_to_int (Succ Zero) = 1
    - nat_to_int (Succ (Succ (Succ Zero))) = 3    
*)

let rec nat_to_int (n : nat) : int = raise (Not_implemented "Not implemented")

(* 
    Exercise: Convert from int to nat
    
    Write a function int_to_nat : int -> nat that converts OCaml integers
    to our inductive natural numbers.
    
    Examples:
    - int_to_nat 0 = Zero
    - int_to_nat 1 = Succ Zero
    - int_to_nat 3 = Succ (Succ (Succ Zero))
*)

let rec int_to_nat (n : int) : nat = raise (Not_implemented "Not implemented")





(* How do you multiply? *)