exception NotImplementedError of string
let todo msg = fun _ -> raise (NotImplementedError msg) [@@warning "-27"]

(* Song: New Math by Tom Lehrer *)




(* Lecture 3: Multiplication and Division *)

(* 
    Fun Fact: India is home to the Bombardier Beetle, which, when threatened 
    releases a rapid, high-temperature (100°C) chemical spray from its abdomen,
    accompanied by an audible "pop" sound.
*)


(* In the last class we saw custom defined int *)

type nat = Zero | Succ of nat

let a : nat = Succ (Succ (Succ (Succ (Succ (Zero)))))
let b : nat = Succ (Succ (Succ (Succ (Zero))))
       

(* Define Addition: *)

let plusZero (n : nat) : nat = n
let plusOne (n : nat) : nat = Succ n
let plusTwo (n : nat) : nat = Succ (Succ n)
let plusThree (n : nat) : nat = Succ (Succ (Succ n))

let plusThree (n : nat) : nat = Succ (plusTwo n)
       
let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 n)

(* Is this addition necessarily correct? *)


(* Proof of correctness of addition using denotational semantics:

    We need to show that [[plus n m]] = [[n]] + [[m]] *)


let rec nat_to_int (n : nat) : int =
  match n with
  | Zero -> 0
  | Succ k -> 1 + nat_to_int k

(* We want to prove: nat_to_int (plus k n) = (nat_to_int k) + (nat_to_int n) *)

let rec int_to_nat (n: int) : nat = 
  match n with 
  | 0 -> Zero
  | m when (m > 0) -> Succ (int_to_nat (m - 1))
  | _ -> Zero


(* nat_to_int (int_to_nat n) = n ?
  int_to_nat (nat_to_int n) = n ? 

*)




(* Proof by induction on k:

  Base case: k = Zero
    plus Zero n = n
    nat_to_int (plus Zero n) = nat_to_int n
    nat_to_int Zero + nat_to_int n = 0 + nat_to_int n = nat_to_int n

  Inductive step: Assume true for k = k2
    plus (Succ k2) n = Succ (plus k2 n)
    nat_to_int (plus (Succ k2) n) = nat_to_int (Succ (plus k2 n))
                        = 1 + nat_to_int (plus k2 n)
    By induction hypothesis:
     nat_to_int (plus k2 n) = nat_to_int k2 + nat_to_int n
    So,
     nat_to_int (plus (Succ k2) n) = 1 + (nat_to_int k2 + nat_to_int n)
                         = (1 + nat_to_int k2) + nat_to_int n
                         = nat_to_int (Succ k2) + nat_to_int n

  Therefore, by induction, the denotational semantics of plus matches integer 
  addition.
*)




(* Proof of correctness of addition using axiomatic semantics:

    To prove that addition is correct, we need to show that it satisfies the 
    following properties: 
    
    1. Zero is the identity element for addition.
    2. The successor of a natural number n is the result of adding one to n.

    Succ (n) = plus (Succ Zero) n
*)


(* How do we define subtraction? *)

(* 
    Subtraction is not defined for all natural numbers, as it can lead to 
    negative results. However, we can define subtraction in a way that it only 
    returns a natural number if the result is non-negative. We can define 
    subtraction recursively as follows:

    - For any natural number n, subtracting Zero gives n.
    - For any natural number n, subtracting Succ k gives minus k n if n is 
      greater than Succ k, otherwise Zero.
*)

let rec minus (n : nat) (m : nat) : nat = 
  match n with 
  | Zero -> Zero
  | Succ n' -> 
    match m with 
    | Zero -> n 
    | Succ m' -> minus n' m'
  
(* Dry Run 

    minus (Succ (Succ (Succ Zero))) (Succ Zero)
  = minus Succ (Succ (Zero)) Zero
  = minus Succ (Succ (Zero))

    minus (Succ Zero) (Succ (Succ (Succ Zero))) 
  = minus (Zero) (Succ (Succ (Zero)))
  = Zero
*)

(* How do we define multiplication? *)

(* 
    Multiplication can be defined recursively as follows:
    - For any natural number n, multiplying by Zero gives Zero.
    - For any natural number n, multiplying by Succ k gives n + (n * k). *)
    
let rec mult (n : nat) (m : nat) : nat = 
  match n with
  | Zero -> Zero
  | Succ n' -> plus (mult n' m) m

  (* mult Succ (n') m = plus (mult n' m) m *)




(* How do we define division? *)

(* 
    Division can be defined recursively as follows:
    - For any natural number n, dividing by Zero is undefined.
    - For any natural number n, dividing by Succ k gives the largest natural 
    number m such that m * (Succ k) <= n.  
*)


let rec div (n : nat) (m : nat) : nat = 
  match n with
  | Zero -> Zero
  | Succ n' -> minus (div n' m) m


(* Dry run

    div 4 2 
  = minus (div 3 2) 2
  = minus (minus (div 2 2) 2) 2
  = minus (minus (minus (div 1 2) 2) 2) 2
  = minus (minus (minus (minus (div 0 2) 2) 2) 2) 2
  = minus (minus (minus (minus 0 2) 2) 2) 2
  = minus (minus (minus 0 2) 2) 2
  = minus (minus 0 2) 2
  = minus 0 2
  = 0

*)

let rec compare (n : nat) (m : nat) : bool = todo "Implement it"






(* A First and Naive Implementation of Division *)
let rec div (n: nat) (m: nat): nat =
    match n with
    | Zero -> Zero
    | _ -> Succ (div (minus n m) m)

(* IDEA: n / m = 1 + ((n - m)/m) *)



(*  div 4 2 
  = Succ (div (minus 4 2) 2)
  = Succ (div 2 2)
  = Succ (Succ (div (minus 2 2) 2))
  = Succ (Succ (div 0 2))
  = Succ (Succ 0)

  Problematic Case: m = 0

    div 4 0
  = Succ (div (minus 4 0) 0)
  = Succ (div 4 0)

  ...
  ...
  = Succ (Succ (Succ ... (Succ (div 4 0))))



*)


let rec div (n: nat) (m: nat): nat =
    match n with
    | Zero -> Zero
    | _ -> 
      match m with 
      | Zero -> Zero
      | _ -> Succ (div (minus n m) m)

(* 

    div 2 4
  = Succ (div (minus 2 4) 4)
  = Succ (div 0 4)
  = Succ (0)

    div 3 2
  = Succ (div (minus 3 2) 2)
  = Succ (div 1 2)
  = Succ (Succ (div 0 2))
  = Succ (Succ 0)


*)






(* Division is especially difficult to define correctly. *)

(* Case Study IV: Pentium FDIV bug (1994)

    In 1994, a bug in the Pentium processor's floating-point division unit 
    caused incorrect results in some calculations. The bug was caused by a 
    hardware error in the division unit. The bug was not detected until 1996, 
    when it was discovered that the bug was causing incorrect results in some 
    calculations. 

    In particular, dividing 4,195,835 by 3,145,727 gave 1.33382
    instead of the correct result of 1.33373.

    Intel said it incurred "a $475 million pre-tax charge ... to recover 
    replacement and write-off of these microprocessors."
*)








(* You can define new exception messages such as: *)
exception Custom_Division_by_zero

let rec div_2 (n: nat) (m: nat): nat =
    match (n, m) with
    | (_ , Zero) -> raise Custom_Division_by_zero
    | (Zero, _) -> Zero
    | (Succ n', _) -> Succ (div_2 (minus n m) m)
;;


(* However, this code is still wrong, for example consider: 

  div_2 two three = Zero 

  (this can be stated using assertions)

We want to differentiate between the following:
'minus two three = Zero' and 'minus three three = Zero' *)

(* Let us introduce comparison! *)
let rec less_than (n: nat) (m: nat): bool = 
    match (n, m) with 
    | (_, Zero) -> false
    | (Zero, _) -> true
    | (Succ n', Succ m') -> less_than n' m''





let rec safe_and_correct_div (n: nat) (m: nat): nat =
    match (n, m) with
    | (_, Zero) -> raise Custom_Division_by_zero
    | (Zero, _) -> Zero
    | _ when (less_than n m) -> Zero  (* Stop when n < m *)
    | _ -> Succ (safe_and_correct_div (minus n m) m)


  (* Correctness Criteria: ???

    q = (div n m)

    If m is non-zero, then: 0 <= (n - (m * (div n m))) < m 
  *)





(* But I don't like this. Let us clean it up *)

let rec clean_div (n: nat) (m: nat): nat =
  match (n, m) with
  | (_, Zero) -> raise Custom_Division_by_zero
  | (Zero, _) -> Zero
  | _ ->
      match (minus n m) with
      | Zero when not (n = m) -> Zero    (* not (n = m) is same as (n <> m) *)
      | diff -> Succ (clean_div diff m)  (* Only count full subtractions *)


(* Our code ends with an exception as soon as one is raised! The same will 
happen with nested functions. The world breaks with exceptions! *)


(* We instead want to support a way to handle exceptions well. *)

(* Case Study VI: USS Yorktown (CG-48).

In 1996, the U.S. Navy's Smart Ship initiative, aiming to automate key key 
operations and cut crew costs by 10% and save an estimated $2.8 million annually, 
decided to use a Windows NT 4.0-based network to manage vital functions like 
navigation, engine monitoring, and fuel control. 

However, during maneuvers in 1997, a crew member input a zero into a database, 
triggering a division-by-zero error in the Remote Database Manager (RDM).  

Since other Smart Ship systems were dependent on RDM availability across the 
LAN, these other SMCS components including ones controlling the motor and 
propulsion machinery began to fail in a domino-like sequence until the ship 
stopped dead in the water.
*)



(* For handling exceptions, we will use option types! Option types in OCaml 
represent values that may or may not be present. They are defined as 

    type 'a option = None | Some of 'a' where 

'a' is any type. This is useful for functions that may not return a valid 
result, such as division by zero.

*)

let safe_minus_int (n: int) (m: int): int option =
    if n >= m then Some (n - m) else None
;;

let safe_minus_nat (n: nat) (m: nat): nat option =
  match (n, m) with 
  | (Zero, Zero) -> Some Zero
  | (Zero, _ ) -> None
  | (_, Zero) -> Some n
  | (Succ n', Succ m') -> safe_minus_nat n' m'


let safe_reciprocal (n: float): float option =
    if n = 0.0 then None else Some (1.0 /. n)
;;







let rec new_safe_div (n: nat) (m: nat) : nat option = 
  match (n, m) with 
  | (_, Zero) -> None
  | (Zero, _) -> Some Zero
  | (_, _) -> 
    match (safe_minus_nat n m) with 
    | None -> Some Zero
    | Some diff -> Some (Succ (div diff m))

    (* div n m = 1 + div (n - m) m *)


(* What about the remainder? *)




(* But we need a quotient and a remainder. Let us first figure out how we can 
have a function that has two outputs. *)  

(* A tuple is a fixed-size collections of values. For example, a pair of values
written as (a, b), but tuples can have more elements, e.g., (a, b, c).

  Example: A pair of integers
    let p : int * int = (3, 4)

  Example: A triple of different types
    let t : int * string * bool = (42, "hello", true)

  Accessing tuple elements is usually done via pattern matching:
    let (x, y) = p in
    (* x = 3, y = 4 *) 

*)

let min_max (a : int) (b : int) : int * int =
  match (a < b) with
  | true -> (a, b)
  | false -> (b, a) 


let min_max (a : int) (b : int) : int * int =
  match (a, b) with
  | (x, y) when x < y -> (a, b)
  | _ -> (b, a)


(* Now we can define a function that returns both the quotient and the 
remainder of a division operation. *)


let rec divmod (n: nat) (m: nat) : (nat * nat) option = 
  match (n, m) with 
  | (_, Zero) -> None
  | (Zero, _) -> Some (Zero, Zero)
  | (_, _) -> 
    match (safe_minus_nat n m) with 
    | None -> Some (Zero, n)
    | Some diff ->
      match divmod diff m with 
      | None -> None
      | Some (q', r') -> Some (Succ q', r')


    (* Say, previous division gave me None. Then just do None. 
            previous division gave me Some(q' r'). 
            q = Succ q'
            r = r'  
    *)


