(* Lecture 13: The While Loop *)






(* Tail Recursion *)

let rec factorial (n : int) : int = 
  match n with
  | 0 -> 1
  | _ -> n * factorial (n - 1)

let rec factorial_helper (a : int) (n : int) : int =
  match n with
  | 0 -> a
  | _ -> factorial_helper (a * n) (n - 1)


let factorial_tail (n : int) : int = factorial_helper 1 n











let factorial (n: int) : int =
  (* references *)
  let a = ref 1 in
  for i = 1 to n do
    (* assignments and deref. *)
    a := !a * i
  done;
  !a
;;















(* How do you prove the correctness of a program with a for loop? By defining 
properties on the state of the program. *)

(* A loop invariant is a property (or condition) of the state of the program 
that:

1. Holds before the loop starts.
2. Remains true after every iteration of the loop.
3. When the loop ends, it helps prove that the program produces the correct 
result. *)


(* Requires: n >= 1
   Ensures: output = n! 
*)

let factorial (n: int) : int =
  let a = ref 1 in
  (* Loop Invariant: At the start of each iteration !a = factorial(i - 1) *)
  for i = 1 to n do
    (* At the start of iteration i:
       Invariant holds: !a = (i - 1)!
       We are about to compute i-th factorial by multiplying !a by i.
    *)
    a := !a * i;

    (* After this step:
       Invariant holds: !a = factorial(i)
    *)
  done;

  (* When the loop terminates:
     i = n + 1
     Therefore, !a = factorial(n)
  *)
  !a
;;

(* Loop invariants are similar to proofs using the Principle of Mathematical 
Induction. Initialization, that is, the condition must hold before the start 
of the loop is like the base case. The Maintenance Step, that is, if the 
condition holds before the start of an iteration it must hold at the end of 
it is like the inductive step. And finally, the Termination Step completes the 
proof. Going forward, every program with a for loop must have a loop invariant. 
Or rather, first write a loop invariant, and then write a loop. *)












(* Let us write a loop for the following: *)

let rec fib (n : int) : int =
  match (n <= 0) with 
  | true -> 1
  | false ->
    match (n = 1) with 
    | true -> 1
    | false -> fib (n - 1) + fib (n - 2)
;;










(* 

Requires: n >= 0
Ensures: 
      fib 1 = 1
      fib 0 = 1 
      and for all n >= 2, 
      fib n = fib (n - 1) + fib (n - 2)


I need this claim: 
  For all i in 2 to n, 
        fib (i) = fib(i - 1) + fib (i - 2)


Somehow if I can compute and store fib (n - 1) correctly and fib (n - 2) correctly then I can compute fib n correctly.

Then store fib (n - 1) and also store fib (n - 2). 

Let storage box prev store fib (n - 1) and storage box prevprev store fib (n - 2). 


*)



let fib_for (n: int) : int =
  if n <= 0 then 1
  else if n = 1 then 1
  else (
    let prev = ref 1 in (* this is fib 0 *)
    let prevprev = ref 1 in (* this is fib 1 *)
    for i = 2 to n do
      (* Invariant: At the start of iteration 
        i = k,
         !prev = fib(k - 1) and 
         !prevprev = fib(k - 2) 
      *)

      let next = !prev + !prevprev in (* next = fib(k) *)
      prevprev := !prev; (* a = fib(k - 1) *)
      prev := next; (* b = fib(k) *)
      (* Invariant: At the end of iteration i = k,
         !prevprev = fib(k - 1) and !prev = fib(k) *)
    done;
    (* Value of i = n + 1 *)
    !prev (* b = fib(i - 1) = fib(n) *)
  )
;;

















(* How would compute GCD using a loop? *)

let rec gcd (a: int) (b: int) : int =
  if b = 0 then a
  else gcd b (a mod b)
;;


(* Requires: Either a not equal to 0 or b not equal to 0. 
   Ensures:  output d. Such that
             d divides a 
             d divides b
             and for any number d' > d, 
              d' not divides a or d' not divides b.
   *)


(* Digression: 

  Inputs fib (k + 1) and fib (k), are the smallest values that take exactly k steps in GCD. 

*)



(* You do not know how many times to repeat the operation,
   because the number of recursive calls depends on the size of the inputs
   and how quickly they reduce through the modulo operation.
   
   For this, we will use the while loop! *)



















let rec while_loop (condition: bool) (body: unit) =
  if condition then (
    body;
    while_loop condition body
  )
;;


(* How would we write GCD using while loop?

  x = ref a ; 
  y = ref b ;

  While !y <> 0, 
    tmp = !y
    y := !x mod !y 
    x := temp

  !x

*)



















let gcd (a : int) (b : int) : int = 
  if (a = 0 && b = 0) then failwith "gcd: both inputs are zero"
  else
    let x = ref a in 
    let y = ref b in 
    (* Invariant: (gcd !x !y) = (gcd a b).  *)
    while (!y <> 0) do
      let tmp = !y in
      y := !x mod !y; 
      x := tmp
    (* Because gcd  m (n mod m) = gcd n m, 
    invariant holds after the loop  *)
    done
    (* gcd !x !y = gcd a b *)
    (* !y = 0 *)
    (* gcd a b = gcd !x 0 = !x, so return !x*)
    !x
;;







