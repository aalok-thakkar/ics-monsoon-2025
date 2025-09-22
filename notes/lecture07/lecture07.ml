(* Lecture 7: An Algebra of Lists *)

(* 

  Up to this point, we have worked with several native types provided OCaml:
    int
    float
    string
    bool

  We have also introduced some custom types of our own, including:
    day
    bool
    nat

  With these in place, we are now ready to construct more sophisticated data 
  structures. A data structure is a way of organizing and storing data so that 
  it can be accessed and modified efficiently. 

  In this lecture, we will start with a basic data structure called lists. We 
  will also see how lists and natural numbers share an underlying algebraic 
  structure
*)

(* type list = Nil | Cons of (x, list) 

If x is of type 'a, we can say the following: *) 


type 'a mylist = Nil | Cons of 'a * 'a mylist
(* A list is either Nil or Cons(head, tail).  *)
(* Here, 'a denotes an arbitrary type. That is, the list will have elements of 
type 'a. We can instantiate it with say bool, to get something like this: *)


let sample_bool_list: bool mylist = Cons(true, Cons(false, Nil))
let sample_string_list: string mylist = Cons("one", Cons("TWO", Cons("3", Nil)))





(* Observe that this parallels Peano axioms. *)

type nat = Zero | Succ of nat



(* One can map the Nil constructor to Zero and Cons to Succ, giving us the following function: *)

let rec list_to_nat (l: 'a mylist) : nat = 
  match l with
  | Nil -> Zero
  | Cons (h, tail) -> Succ (list_to_nat tail)

(* Let us run it on a simple example: 

      list_to_nat Cons(1, Cons(2, Cons(4, Nil)))
    = Succ (list_to_nat Cons(2, Cons(4, Nil)))
    = Succ (Succ(list_to_nat Cons(4, Nil)))
    = Succ (Succ( Succ (list_to_nat Nil)))
    = Succ (Succ( Succ (Zero)))
*)
(* Observe closely. The function list_to_nat computes the length of the list. *)




(* For ease of notation, we will use native lists instead of explicitly 
spelling out the type constructors Nil and Cons. *)

let empty_list : int list = []
let sample_list: int list = [1; 2]

(* Syntax: *)

(* [1 ; 2; 3] = 1 :: [2; 3] = 1 :: 2 :: [3] = 1 :: 2 :: 3 :: [] *)

let rec length (l: 'a list) : nat = 
  match l with
  | []           -> Zero
  | head :: tail -> Succ (length tail)


(* Let us define some operations on the list *)




let first (l : 'a list): 'a option = 
  match l with 
  | []     -> None
  | h :: t -> Some h





(* If instead we had the natural number type *)
let first_nat (n : nat): bool = 
  match n with 
  | Zero -> false
  | Succ _ -> true


(* analogue: *)
let is_empty (l : 'a list): bool = 
  match l with 
  | []     -> false
  | h :: t -> true












(* How do you combine two lists? *)

(* Note that l1 and l2 have the same type *)
let rec concat (l1: 'a list) (l2: 'a list) : 'a list = 
  match l1 with 
  | [] -> l2
  | h :: t -> h :: (concat t l2)


(* This is denoted by l1 @ l2 *)

(* analogue: *)
let rec concat_nat (n1: nat) (n2: nat) : nat = 
  match n1 with 
  | Zero -> n2
  | Succ t -> Succ (concat_nat t n2)


let rec plus (n: nat) (m: nat) : nat = 
  match n with 
  | Zero -> m
  | Succ n' -> Succ (plus n' m)







(* Exercise: Define cartesian product of lists. *)

let rec cart_prod (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list = 
  match l1 with 
  | [] -> []
  | h :: t -> [] (* TODO implement *)













(* 

   There is a deep similarity between lists and natural numbers. One can 
   better understand it through algebra.


   MONOID STRUCTURE
   
   A monoid is a set M equipped with:
   1. A binary operation (×) : M × M → M (associative)
   2. An identity element e ∈ M such that e × x = x × e = x for all x ∈ M
   
   The associativity law: (a × b) × c = a × (b × c)
   The identity law: e × a = a × e = a



   NATURAL NUMBERS AS A MONOID
   
   The natural numbers form a monoid under addition:
   - Binary operation: addition (+)
   - Identity element: Zero
   - Associativity: (a + b) + c = a + (b + c)
   - Identity: 0 + n = n + 0 = n



   LISTS AS A MONOID
   
   Lists form a monoid under concatenation:
   - Binary operation: concatenation (@)
   - Identity element: empty list []
   - Associativity: (l1 @ l2) @ l3 = l1 @ (l2 @ l3)
   - Identity: [] @ l = l @ [] = l








   Say @ is our implementation of concat. Then the above claim requires the following proofs: 

   1. For all lists l, [] @ l = l @ [] = l

    Proof method: Induction on the length of the list. 

    Property: l @ [] = l. 

    Base Case: length (l) = 0. Then l = []. So [] @ [] = [].

    Inductive Hypothesis: For all lists of length up to k, 
          l @ [] = l. 

    Inductive Step: Consider list (h :: l). This has length = (length l) + 1.

    concat (h :: l) [] = h :: (concat l [])

    By inductive hypothesis, concat l [] = l. 
    Therefore, 

    concat (h :: l) [] = h :: (concat l []) = h :: l

    Hence proved. 







    Proof method: Structural induction on the list. 

    Property: l @ [] = l. 

    Base Case: list is empty. Then [] @ [] = [].

    Inductive Hypothesis: For an arbitary list l, say l @ [] = l

    Inductive Step: Consider list (h :: l).

    concat (h :: l) [] = h :: (concat l [])

    By inductive hypothesis, concat l [] = l. Therefore, 

    concat (h :: l) [] = h :: (concat l []) = h :: l

    Hence proved. 


   
   2. For all lists l1, l2, l3, (l1 @ l2) @ l3 = l1 @ (l2 @ l3)

   (* Exercise: Follow the above proof, and use structural induction to prove the associativity of concatenation *)


   




   HOMOMORPHISM BETWEEN MONOIDS
   
   A homomorphism h: M → N between monoids (M, (×), e_M) and (N, (+), e_N) is 
   a function that preserves:
   1. The binary operation: h(a × b) = h(a) + h(b)
   2. The identity element: h(e_M) = e_N


   Exercise:  Prove that the length function is a monoid homomorphism from the 
   list monoid to the natural number monoid.

*)




(* 
   PROOF THAT LENGTH IS A HOMOMORPHISM
   
   We need to show:
   1. length([]) = Zero 
   2. length(l1 @ l2) = length(l1) + length(l2) 
   
   Proof of (1): By definition, length([]) = Zero
   
   Proof of (2): By structural induction on l1
   Base case: l1 = []
     length([] @ l2) = length(l2) = Zero + length(l2) = length([]) + length(l2)
   
   Inductive case: l1 = h :: t
     length((h :: t) @ l2) = length(h :: (t @ l2)) = Succ(length(t @ l2))
     = Succ(length(t) + length(l2)) = Succ(length(t)) + length(l2)
     = length(h :: t) + length(l2) 





   COMMUTATIVE DIAGRAM
   
   The homomorphism property can be visualized as a commutative diagram:
   
   (List × List) ──concat──>   List
         │                      │
      length                  length
         ↓                      ↓
    (Nat × Nat)  ──plus_nat──> Nat
   
   This diagram commutes! 

   At a high level, this means that applying the length function after 
   concatenating two lists yields the same result as first taking the lengths 
   of the individual lists and then adding them. In other words, the order in 
   which we apply the operations (concatenation vs. length and addition) does 
   not affect the final outcome. 






   In computer science, such commutative diagrams and homomorphism properties 
   reveal structural similarities between different data types and operations. 
   Recognizing these correspondences allows us to reason about programs more 
   abstractly, transfer properties and optimizations from one domain to 
   another, and design generic, reusable algorithms.



   ADDITIONAL MONOID EXAMPLES
   
   Other examples of monoids in programming:
   1. Strings under concatenation with empty string as identity
   2. Booleans under AND with true as identity
   3. Booleans under OR with false as identity
   4. Functions under composition with identity function as identity
   5. Sets under union with empty set as identity
   6. Integers under multiplication with 1 as identity
*)









(* Now, let us work with lists *)






(* Find the ith element of a list *)

(* 
   Requires: 0 <= i < length (l). 
   Ensures: Returns Some x if x is the element at index i (0-based); 
            returns None if i is out of bounds.
*)
let rec nth (l : 'a list) (i : int) : 'a option = 
    match (l, i) with 
    | ([], _)           -> None 
    | (h :: _, 0)       -> Some h 
    | (_, n) when n < 0 -> None
    | (_ :: t, n)       -> nth (t) (i - 1)



(* Check if an element is present in the list *)

(*
   Requires: true (no precondition).
   Ensures: Returns Some i if (nth l i = x); None otherwise.
*)
let rec where_at (v : 'a) (l : 'a list) : int option = 
  match l with 
  | [] -> None
  | h :: t -> 
    match (h = v) with 
    | true -> Some 0 
    | false -> 
      match (where_at v t) with 
      | None -> None 
      | Some n -> Some (n + 1)

      (* Exercise: Fix this ugly code. Have fewer match statements. Make it readable. *)


      (* How can we prove that where_at is correct? 

      where_at v l = Some i  if and only if  nth l i = v

      Using structural induction. *)





(* Reverse a list *)




(* What will be the blueprint? *)












(*
   Requires: true (no precondition).
   Ensures: Returns a list l' such that: 
   
                   nth l' i = nth l (n - i - 1)
     
     
    where: 0 <= i < length l 

    and length l' = length l.
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

--- Conclusion:

By structural induction, the property holds for all finite lists.

Thus reverse is correct.

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