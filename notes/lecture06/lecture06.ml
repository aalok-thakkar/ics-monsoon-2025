(* Lecture 6: Who builds a skyscrapper without a blueprint? *)



type nat = Zero | Succ of nat

(* How do we define subtraction? *)


let rec minus (n : nat) (m : nat) : nat option =
  match (n, m) with 
  | (Zero, Zero)       -> Some Zero
  | (Zero, Succ m')    -> None
  | (_, Zero)    -> Some n
  | (Succ n', Succ m') -> minus n' m'











let rec div (num: nat) (denom: nat): nat =
  match num with
  | Zero -> Zero
  | Succ n' -> 
    match (minus (div n' denom) denom) with 
    | None      -> Zero
    | Some diff -> diff




let rec div2 (num: nat) (denom: nat): nat =
  match num with
  | Zero -> Zero
  | _ -> 
    match (minus num denom) with 
    | None -> Zero 
    | Some diff -> Succ (div2 diff denom)





let rec div3 (num: nat) (denom: nat): nat option =
  match denom with 
  | Zero -> None                (* This handles division by zero ... *)
  | _ -> 
    match num with
    | Zero -> Some Zero
    | _ -> 
      match (minus num denom) with 
      | None -> Some Zero 
      | Some diff -> 
        match (div3 diff denom) with 
        | None -> None
        | Some q -> Some (Succ q)










(* Is this good enough? 

  1. Does it terminate? 
  2. Does it always return a correct result?
  3. Is it efficient?
  4. Is the implementation robust to invalid inputs?
  5. Does it handle all edge cases, such as division by zero?
  6. Is the code readable and maintainable?




*)








  

(* Leslie Lamport, recipient of the 2013 Turing Award, has long advocated for
   rigorous specifications in software development. His fundamental insight is
   that specifications serve as a contract between the implementer and user of
   code, clearly defining expected behavior.

   Without specifications, programmers often:
   - Make incorrect assumptions about how code should behave
   - Miss edge cases and error conditions 
   - Create brittle implementations tied to specific use cases
   - Have difficulty maintaining and evolving code over time





   "Architects draw detailed plans before a brick is laid or a nail is hammered. 
   But few programmers write even a rough sketch of what their programs will do 
   before they start coding. We can learn from architects."
   - Leslie Lamport

 Full Talk: https://www.youtube.com/watch?v=iCRqE59VXT0 
 
 *)

(* In this course, our blueprints take the form of requires/ensures clauses:
   - requires: preconditions that must be true before calling the function
   - ensures: postconditions guaranteed to be true after the function returns


    More formally, they take the form of Hoare triples:

       { P }   C   { Q }

   - P (requires): the precondition, what must hold before execution
   - C: the command (or program fragment)
   - Q (ensures): the postcondition, what is guaranteed after execution


   This is a contract: if the caller ensures P, the program guarantees Q.
*)






(* 

Blueprint for division 

Requires: denom <> Zero 
Ensures:  division(num, denom) = q then ... 

exists some r such that r < denom, and q * denom + r = num

*)






(* For the purpose of correctness, let us also compute the remainder *)













(* Let us first figure out how we can have a function that has two outputs. *)  

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
  | true  -> (a, b)
  | false -> (b, a)


(* Now we can define a function that returns both the quotient and the 
remainder of a division operation. *)




(* How would you begin? *)




let rec divmod (num : nat) (denom : nat) : (nat * nat) option =
(* 
   Requires : denom <> Zero 
   Ensures :  if divmod num denom = (q, r) then 
                  (i)  q * denom + r = num 
                 (ii)  r < denom 
*) 
  match denom with 
  | Zero -> None
  | _ -> 
    match minus num denom with 
    | None -> Some (Zero, num)
    | Some diff -> 
      match divmod diff denom with 
      | None -> raise CUSTOM_ERROR
      | Some (q, r) -> Some (Succ q, r)
*)


(* Inductive proof of correctness: 

   (i) Base Case: 
          num < denom

          In this case, minus num denom = None (by definition of minus)
          Therefore, quotient = Zero
                     remainder = num 

          Then Zero * denom + num = num follows from arithmetic. 

   (ii) Induction Hypothesis:

           For all num up to some k, property one and two hold... 

   (iii) Inductive Step: 

          Consider Succ k, then minus (Succ k) denom < k as denom <> Zero. 

          Then by induction hypothesis 

          q * denom + r = (minus (Succ k) denom)

          Therefore     (after 3-4 steps... )

          (Succ q) * denom + r = Succ k


          Which is what was returned in line 221.


*)












  (* Case Study IX: seL4 Microkernel
    

  The seL4 microkernel, developed by NICTA (National ICT Australia) and first 
  released in 2009, is a pioneering project in applying formal verification to 
  real-world operating systems. The project produced the first general-purpose 
  OS kernel whose C implementation was fully mechanically proved to satisfy its
  formal specification. This breakthrough established strong guarantees of 
  memory safety, process isolation, and the absence of common bugs such as 
  buffer overflows and null-pointer dereferences. Over the following years, 
  seL4 was open-sourced (2014) and adopted in safety- and security-critical 
  domains, including aerospace and defense. 

  *)



(* 
   Hoare triples may seem rigid, but they are most important for "vibe coding":
   - When you *know* the contract, coding becomes more playful.
   - You explore ideas within a safe sandbox, confident your invariants hold.
   - Specifications remove the mental load of "did I miss an edge case?"—
     letting you stay in flow.

   In short: rigorous contracts (Hoare triples) enable creativity, liberating
   one from the fear of implementation level bugs. 
*)



(* Coincidentally, yesterday was an important celebration. 

September 9th marks the anniversary of the first recorded “bug” in computing 
history. In 1947, engineers working on the Mark II Aiken Relay Calculator at 
Harvard discovered that a moth had short-circuited one of its relays. The 
insect was carefully taped into the logbook with the note: “First actual case 
of bug being found.” This lighthearted documentation gave rise to the enduring 
use of the terms bug and debugging in the world of computing.



“If debugging is the process of removing bugs, 
then programming must be the process of putting them in.”

- Edgar Dijkstra 

*)