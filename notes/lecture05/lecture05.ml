(* Lecture 5: Division is Repeated Subtraction *)










(* We saw two different algorithms for exponentiation: *)

let rec exp (base : int) (power : int) : int =
  match power with
  | 0 -> 1
  | _ when power < 0 -> raise (Invalid_argument "exponent must be non-negative")
  | _ -> base * exp base (power - 1)



(* Let us introduce a new function T that tracks how much time it takes for the function exp. 

T(e) = time it takes to multiply + time it takes to compute (exp b (e - 1))
        = 1 + T(e - 1)
        = 1 + 1 + T(e - 2)
        = k + T(e - k)
        = e + T(e - e) = e + T(0) = e + 1
        
T(e) <= e + 1 = O(e)

So exp is linear in the power/exponent

*)

let rec pingala_exp (base : int) (power : int) : int =
  match power with
  | 0 -> 1
  | n when n < 0 -> raise (Invalid_argument "exponent must be non-negative")
  | n -> 
      let half = pingala_exp base (n/2) in
      match n mod 2 with
      | 0 -> half * half
      | _ -> base * half * half

(* Exercise: What if I consider base 3? *)









(* Let us introduce a new function T that tracks how much time it takes for the function pingala_exp. 

T(e) =   time it takes to compute half
       + time it takes to match 
       + time it takes to multiply

     <= T(e/2) + 4
      = (T(e/4) + 4) + 4
      = ((T(e/8) + 4)) + 4) + 4
      = ... 
      = T(e/(2^k)) + 4k
      = T(e/(2^ log_2 e)) + 4*log_2 e
      = T(1) + 4*log_2 e
      <= 10 + 4*log_2 e
      = O(log e)
  
      Let us say T(1) <= some constant (like 10)

*)


(* Let's compare their performance: *)


let time_function (f : int -> int -> int) (x : int) (y : int) : float =
  let start_time = Sys.time () in
  let _ = f x y in
  let end_time = Sys.time () in
  end_time -. start_time

let () =
  Printf.printf "exp (simple recursive):\n";
  let t1 = time_function exp 3 500 in
  Printf.printf "runtime: %f seconds\n\n" t1;
  Printf.printf "pingala_exp (Pingala's method):\n";
  let t2 = time_function pingala_exp 3 500 in
  Printf.printf "runtime: %f seconds\n\n" t2

(* The simple recursive version is O(n) as it makes n recursive calls.
   Pingala's method is O(log n) as it halves the power each time. *)


(* 

Proof that pingala_exp is O(log n):

   Let T(n) be the number of recursive calls made by pingala_exp for power n.
   
   For even n:
   T(n) = 1 + T(n/2)    [1 call plus recursion on n/2]
   
   For odd n: 
   T(n) = 1 + T((n-1)/2) [1 call plus recursion on (n-1)/2]
   
   In both cases, the next recursive call operates on roughly n/2.
   
   Starting with n, the sequence of values is approximately:
   n -> n/2 -> n/4 -> n/8 -> ... -> 1
   
   The number of steps to reach 1 is log₂(n).
   
   Therefore T(n) = O(log n).
   
   This matches our empirical timing results showing pingala_exp is much 
   faster than the O(n) simple recursive version for large powers. 
   
*)














(* We defined addition, multiplication, and exponentiation. 



Now we will define 
subtraction, division, and modulus. *)








(* In the previous classes we have seen custom defined nat *)

type nat = Zero | Succ of nat

(* Helper functions for creating natural numbers *)
let zero = Zero
let one = Succ Zero
let two = Succ (Succ Zero)
let three = Succ (Succ (Succ Zero))
let four = Succ (Succ (Succ (Succ Zero)))

(* Helper function to convert nat to int for testing *)
let rec nat_to_int (n: nat): int =
  match n with
  | Zero -> 0
  | Succ n' -> 1 + nat_to_int n'
       
let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 n)


(* How do we define subtraction? *)

(* 
    Subtraction is not defined for all natural numbers, as it can lead to 
    negative results. So what is the blueprint for it?











    - For any natural number n, subtracting Zero gives n.
    - For any natural number Succ (n), subtracting Succ k gives
                  minus k n 
      if n is greater than k, otherwise ??.
*)

let rec minus (n : nat) (m : nat) : nat =
  match m with 
  | Zero -> n 
  | Succ m' -> 
    match n with 
    | Zero -> Zero
    | Succ n' -> minus n' m'


let rec minus2 (n : nat) (m : nat) : nat =
  match (n, m) with 
  | (Zero, Zero)       -> Zero
  | (Zero, Succ m')    -> Zero
  | (Succ n', Zero)    -> n
  | (Succ n', Succ m') -> minus n' m'

(* Dry Run

    minus four two 
  = minus S.S.S.S.Z S.S.Z
  = minus S.S.S.Z S.Z
  = minus S.S.Z Z
  = S.S.Z
  = two

*)









(* How do we define multiplication? *)

let rec mult (n: nat) (m: nat): nat =
  match n with
  | Zero -> Zero
  | Succ n' -> plus m (mult n' m)

(* How do we define division? *)

(* 
    Division is just repeated subtraction?
*)

let rec div (num: nat) (denom: nat): nat =
  match num with
  | Zero -> Zero
  | Succ n' -> minus (div n' denom) denom



(* Dry run

    div 4 2 

*)









(* What is division? *)




(* IDEA: n / m = 1 + ((n - m)/m) *)



(* A First and Naive Implementation of Division *)
let rec div (num: nat) (denom: nat): nat =
    match num with
    | Zero -> Zero
    | _ -> Succ (div (minus num denom) denom)


(*    div four two
   =  div S.S.S.S.Z S.S.Z
   =  Succ (div (minus S.S.S.Z S.S.Z) S.S.Z)
   =  Succ (div S.S.Z S.S.Z) 
   =  Succ (Succ (div (minus S.S.Z S.S.Z) S.S.Z) )
   =  Succ (Succ (div Z S.S.Z))
   =  Succ (Succ Z)
   =  two


   div five two
   =  div S.S.S.S.S.Z S.S.Z
   =  Succ (div (minus S.S.S.S.Z S.S.Z) S.S.Z)
   =  Succ (div S.S.S.Z S.S.Z) 
   =  Succ (Succ (div (minus S.S.Z S.S.Z) S.S.Z) )
   =  Succ (Succ (div S.Z S.S.Z))
   =  Succ (Succ (Succ (div (minus S.Z S.S.Z) S.S.Z)))
   =  Succ Succ Succ (div Z S.S.Z)
   =  Succ Succ Succ Z
   = three



    div four zero 
  = Succ (div (minus four zero) zero)
  = Succ (div four zero)
  = Succ (Succ (div four zero))
  = ....
  = Succ (Succ (Succ (Succ ....)))





  



*)


let div2 (num: nat) (denom: nat) : nat = 
  match denom with 
  | Zero -> Zero 
  | _ -> div num denom

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

(* Case Study VII: Pentium FDIV bug (1994)

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




(* We want to say div num denom = Zero if num < denom *)

let rec less_than (n : nat) (m : nat) : bool = 
  match (n, m) with 
  | (Zero, Zero)       -> false
  | (Zero, Succ m')    -> true
  | (Succ n', Zero)    -> false
  | (Succ n', Succ m') -> less_than n' m'



let rec div3 (num: nat) (denom: nat) : nat = 
  match denom with 
  | Zero -> Zero 
  | _ -> 
    match less_than num denom with 
    | true -> Zero 
    | false -> Succ (div3 (minus num denom) denom)






(* Define a custom exception for division by zero *)
exception Custom_Division_by_zero

(* 
   div_2: nat -> nat -> nat
   Requires: m is a natural number
   Ensures: Returns the quotient of n divided by m using repeated subtraction.
            Raises Custom_Division_by_zero if m = Zero.
            Returns Zero if n = Zero.
            This implementation does not check if n < m, so may overcount.
*)







let rec div4 (num: nat) (denom: nat) : nat = 
  match denom with 
  | Zero -> raise Custom_Division_by_zero
  | _ -> 
    match less_than num denom with 
    | true -> Zero 
    | false -> Succ (div4 (minus num denom) denom)


(* Dry Run 

   div4 four zero 
   div4 five two 
   div4 four two 
*)



















(* But now, is this correct? What is the correctness criteria? *)





  (* 

    q = (div n m)



    If m is non-zero, then:
     (i)  q * m  + remainder = n
     (ii) 0 <= remainder < m


  *)





let rec div4 (num: nat) (denom: nat) : nat = 
  (* 
  
  Requires: denom <> 0 
  Ensures:  if q = div4 num denom, then 
             there exists remainder < denom such that 
             
  
           (q * denom) + remainder = num
  *)
  match denom with 
  | Zero -> raise Custom_Division_by_zero
  | _ -> 
    match less_than num denom with 
    | true -> Zero 
    | false -> Succ (div4 (minus num denom) denom)









(* But I don't like this. Let us clean it up *)

let rec clean_div (n: nat) (m: nat): nat =
  match (n, m) with
  | (_, Zero) -> raise Custom_Division_by_zero
  | (Zero, _) -> Zero
  | _ ->
      match (less_than n m) with
      | true -> Zero 
      | false -> Succ (clean_div (minus n m) m)


(* Our code ends with an exception as soon as one is raised! The same will 
happen with nested functions. The world breaks with exceptions! *)








(* We instead want to support a way to handle exceptions well. *)

(* Case Study VIII: USS Yorktown (CG-48).

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

    type 'a option = None | Some of 'a' 
    
    where 'a' is any type. This is useful for functions that may not return a valid 
result, such as division by zero.

*)






let safe_minus_int (n: int) (m: int): int option =
  match n >= m with
  | true -> Some (n - m)
  | false -> None





let rec safe_minus_nat (n: nat) (m: nat): nat option =
  match (n, m) with 
  | (Zero, Zero) -> Some Zero
  | (Zero, _ ) -> None
  | (_, Zero) -> Some n
  | (Succ n', Succ m') -> safe_minus_nat n' m'


let safe_reciprocal (n: float): float option =
  match n with
  | 0.0 -> None
  | _ -> Some (1.0 /. n)

















let rec new_safe_div (n: nat) (m: nat) : nat option = 
  match (n, m) with 
  | (_, Zero) -> None
  | (Zero, _) -> Some Zero 
  | (_, _) -> 
    match less_than n m with 
    | true -> Some Zero 
    | false -> 
      match (new_safe_div (minus n m) m) with 
      | None -> raise Custom_UNSAFE_IMPLEMENTATION_ERROR
      | Some x -> Succ x


  
  
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
  match (a, b) with
  | (x, y) when x < y -> (a, b)
  | _ -> (b, a)


(* Now we can define a function that returns both the quotient and the 
remainder of a division operation. *)


let rec divmod (n: nat) (m: nat) : (nat * nat) option = None
(* TODO implement this *)


    (* Say, previous division gave me None. Then just do None. 
            previous division gave me Some(q' r'). 
            q = Succ q'
            r = r'  
    *)


