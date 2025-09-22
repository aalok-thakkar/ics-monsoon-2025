(* Lecture 8: Higher-order Functions *)


(* Reverse a list *)

(*
   Requires: true.
   Ensures: Returns l' with length equal to l, where for all 0 <= i < length l,
            nth l' i = nth l (length l - i - 1).
*)

let rec reverse (l : 'a list) : 'a list = 
  match l with 
  | [] -> []
  | h :: t -> (reverse t) @ [h]

(* 
   1. Is this correct? 
   2. Does it terminate? 
   3. What is the time complexity? 
*)


(* Proof of correctness. 

We want to prove that for every list l of length n:

   reverse l = l'  where
     - length l' = length l
     - for all 0 <= i < n,   nth l' i = nth l (n - i - 1)

This says l' is exactly l in reverse order.

We proceed by structural induction on l. 

--- Base Case: l = []

reverse [] = [] by definition.

- length [] = 0 = length l
- there is no i with 0 <= i < 0, so the nth property holds vacuously.

So the claim is true for [].

--- Inductive Hypothesis (IH):

Assume for some list l of length n,

   reverse l = l' such that
     length l' = n
     and for all 0 <= i < n, nth l' i = nth l (n - i - 1).

--- Inductive Step: list of form (h :: l)

We must show the property for h :: l.

reverse (h :: l) 
= (reverse l) @ [h]             (by definition)

Length:
  length ((reverse l) @ [h])
= length (reverse l) + 1
= length l + 1                  (by IH)
= length (h :: l).              (✓)

Now check the nth property.

Let n = length l.  Then length (h :: l) = n + 1.

Take arbitrary i with 0 <= i <= n.

Case 1: i = n.
  Then we ask: nth ((reverse l) @ [h]) n.
  By definition of list concatenation, the last element of (reverse l) @ [h] is h.
  So nth ((reverse l) @ [h]) n = h.
  On the other side:
    nth (h :: l) ((n+1) - n - 1) = nth (h :: l) 0 = h.
  Both sides match. (✓)

Case 2: 0 <= i < n.
  Then i indexes into reverse l part of (reverse l) @ [h].
  So nth ((reverse l) @ [h]) i = nth (reverse l) i.
  By IH:
    nth (reverse l) i = nth l (n - i - 1).
  On the other side:
    nth (h :: l) ((n+1) - i - 1)
  = nth (h :: l) (n - i).
  Since n - i > 0, this is in the tail:
  = nth l (n - i - 1).
  Which matches the IH result. (✓)

Therefore the property holds for h :: l.


By structural induction, the property holds for all finite lists.


--- Termination:

Each recursive call is on the tail of the list, which is strictly smaller.
Hence recursion terminates for all finite input lists.

--- Time Complexity:

Let n = length l.

reverse l makes one recursive call on a sublist of length n-1,
and then appends a singleton list. 
The append operation ( @ ) costs O(n).
So recurrence is T(n) = T(n-1) + O(n).
Thus T(n) = O(n^2).

*)


(* What problems can we solve with just this? *)















(* Can we check if a number is a palindrome? *)











let rec int_to_list_helper (n : int) (acc : int list) : int list =
  match n with
  | 0 -> acc
  | _ -> int_to_list_helper (n / 10) ((n mod 10) :: acc)





(* Check if an integer is a palindrome *)
let is_palindrome (n : int) : bool =
  match n with
  | _ when n < 0 -> false
  | _ -> (int_to_list_helper n []) = reverse (int_to_list_helper n []))




(* List reverse is offered as List.rev *)







(* Sum and Product of a List *)
let rec sum_list (l: int list) : int =
  match l with
  | [] -> 0
  | h :: t -> h + sum_list t




let rec mult_list (l: int list) : int =
  match l with
  | [] -> 1
  | h :: t -> h * sum_list t













(* Now let us do the opposite of length. Convert a number into a list *)



(* 
   Requires: n >= 0.
   Ensures: Returns the list [1; ...; n].
*)

let rec init_list (n : int) : int list = 
  match (n <= 0) with 
  | true  -> []
  | false -> (init_list (n - 1)) @ [n]










let factorial (n: int) : int = mult_list (init_list n)








(* https://projecteuler.net/problem=1 *)

(* Problem 1: 

If we list all the natural numbers below 10 that are multiples of 3 or 5, we 
get 3, 5, 6 and 9. The sum of these multiples is 23. Find the sum of all the 
multiples of 3 or 5 below 1000.

*)











let rec filter_multiples_of_3 (l : int list) : int list = 
  match l with 
  | [] -> []
  | h :: t -> 
    match (h mod 3 = 0) with 
    | true -> h :: (filter_multiples_of_3 t)
    | false -> filter_multiples_of_3 t













let rec filter_multiples_of_5 (l : int list) : int list =
  match l with 
  | [] -> []
  | h :: t -> 
    match (h mod 5 = 0) with 
    | true -> h :: (filter_multiples_of_5 t)
    | false -> filter_multiples_of_5 t







(* Can I just: 
     1. filter multiples of 3 and sum them up
     2. filter multiples of 5 and sum them up
     3. and just add the two sums? 
*)






let rec filter_multiples_of_3_or_5 (l : int list) : int list =
  match l with 
  | [] -> []
  | h :: t -> 
    match ((h mod 3 = 0) || (h mod 5 = 0))  with 
    | true -> h :: (filter_multiples_of_3_or_5 t)
    | false -> filter_multiples_of_3_or_5 t





(* Final answer for Project Euler Problem 1. Expected: 233168. *)
let euler1_answer : int = sum_list (filter_multiples_of_3_or_5 (init_list 999))
let () = Printf.printf "%d\n" euler1_answer ;;










(* Syntax: Pipes *)


(*       x |> f = f (x)        *)
let cube (x: int): int = x * x * x
let result1 : int = cube (12) + cube (1)
let result2 : int = (12 |> cube) + (1 |> cube)




let euler1_answer : int =
  init_list 1000
  |> filter_multiples_of_3_or_5
  |> sum_list














(* Problem 2: 

Each new term in the Fibonacci sequence is generated by adding the previous two 
terms. By starting with 1 and 2, the first 10 terms will be:
          1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ....


By considering the terms in the Fibonacci sequence whose values do not exceed 
four million, find the sum of the even-valued terms.
*)







(* Write a function to filter out even numbers *)




let rec filter_multiples_of_2 (l : int list) : int list =
  match l with 
  | [] -> []
  | h :: t -> 
    match (h mod 2 = 0)  with 
    | true -> h :: (filter_multiples_of_2 t)
    | false -> filter_multiples_of_2 t







let rec fib_list (first : int) (second : int) (limit : int) : int list =
  match first > limit with
  | true -> []
  | false -> first :: fib_list second (first + second) limit

let euler2_answer : int =
  fib_list 1 2 4000000
  |> filter_multiples_of_2 
  |> sum_list












(* A higher order function takes another function as an input *)



let rec filter (p : 'a -> bool) (l : 'a list) : 'a list =
  match l with 
  | [] -> []
  | h :: t -> 
    match (p h)  with 
    | true -> h :: (filter p t)
    | false -> filter p t







let is_multiple_of_3 (x : int) = (x mod 3) = 0
let is_multiple_of_5 (x : int) = (x mod 3) = 0
let is_multiple_of_3_or_5 (x : int) = (is_multiple_of_3 x) || (is_multiple_of_5 x)


(* Then 

    filter_multiples_of_3 = filter is_multiple_of_3
    filter_multiples_of_5 = filter is_multiple_of_3
    filter_multiples_of_3_or_5 = filter is_multiple_of_3_or_5

*)










(* I am unlikely to use is_multiple_of_3 outside of the context of filter. 
   In such a case OCaml offers a way to locally define a function without 
   giving it a name. 

   Something like: 

   filter (by this particular rule) (this particular list)


   This is called a lambda function
*)



(* Lambda function has no name. It has inputs and outputs. 
Defined locally in a particular scope *)



(* Syntax is: 
      keyword: fun 
      followed by input 
      followed by arrow 
      followed by output
*)




let is_multiple_of_3 = fun x -> (x mod 3) = 0

let is_multiple_of_5 = fun x -> (x mod 5) = 0
let is_multiple_of_3_or_5 = fun x -> (is_multiple_of_3 x) || (is_multiple_of_5 x)



(* And I can therefore write: *)




let filter_multiples_of_3 (l : int list) : int list = 
  filter (fun x -> (x mod 3) = 0) l 

(* Now let us solve Euler's Problem One *)









let euler1_answer : int =
  init_list 1000
  |> filter (fun x -> ((x mod 3) = 0 || (x mod 5) = 0))
  |> sum_list











(* Problem 2: 

Each new term in the Fibonacci sequence is generated by adding the previous two 
terms. By starting with 1 and 2, the first 10 terms will be:
          1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ....


By considering the terms in the Fibonacci sequence whose values do not exceed 
four million, find the sum of the even-valued terms.
*)


















(* Excel as a programming language *)