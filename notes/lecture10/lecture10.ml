(* Lecture 10: More Higher-order Functions *)



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


let rec filter (p : 'a -> bool) (l : 'a list) : 'a list =
  match l with 
  | [] -> []
  | h :: t -> 
    match (p h)  with 
    | true -> h :: (filter p t)
    | false -> filter p t


(* Lambda function has no name. It has inputs and outputs. 
Defined locally in a particular scope *)



(* Syntax is: 
      keyword: fun 
      followed by input 
      followed by arrow 
      followed by output
*)


let is_multiple_of_3 = fun x -> (x mod 3) = 0

(* And I can therefore write: *)

let filter_multiples_of_3 (l : int list) : int list = 
  filter (fun x -> (x mod 3) = 0) l 

(* Now let us solve Euler's Problem One *)

let euler1_answer : int =
  init_list 1000
  |> filter (fun x -> ((x mod 3) = 0 || (x mod 5) = 0))
  |> sum_list












(* Let us also create range. We have seen this before *)
let rec range (a: int) (b: int) : int list =
  match a > b with
  | true -> []
  | false -> a :: range (a + 1) b









(* We can use filter to solve some interesting problems *)






(* When is a number prime? *)





let divisors (n : int) : int list = 
  range 2 (n - 1)
  |> filter (fun x -> n mod x = 0) 







let divisors (n: int) : int list =
  match (n > 1) with 
  | true -> Some filter (fun x -> (n mod x = 0)) (range 2 (n - 1))
  | false -> raise "divisors: input is < 2"

let is_prime (x: int) : bool = (divisors x = [])

let primes_under_bound (n : int) =
  range 2 n 
  |> filter is_prime 













let primes (n: int) : int list = filter (is_prime) (range 1 n)
















(* Sieve of Eratosthenes *)

(*
  The Sieve of Eratosthenes is a classic algorithm for finding all prime 
  numbers up to a given number n. The idea is to start with a list of numbers 
  from 2 to n. The first number in the list is always prime. We then remove all 
  multiples of this prime from the list. We repeat this process with the next 
  number in the list, and so on, until we have processed all numbers up to n.

  For example, to find all primes up to 10:
    Start with: [2; 3; 4; 5; 6; 7; 8; 9; 10]
    2 is prime. Remove all multiples of 2 (except 2): [2; 3; 5; 7; 9]
    3 is prime. Remove all multiples of 3 (except 3): [2; 3; 5; 7]
    5 is prime. No multiples of 5 left to remove.
    7 is prime. No multiples of 7 left to remove.
    The result is [2; 3; 5; 7], which are all the primes up to 10.
*)

let rec sieve (l : int list) : int list = 
  match l with 
  | [] -> []
  | h :: t ->
    t
    |> filter (fun x -> (x mod h <> 0))
    |> sieve 
    

(* Exercise: Explore what happens if you sieve first, and then filter? *)





(* Let us look at two other useful higher order function *)


(* Map *)



let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)

let double (x : int) = x * 2
(* map double [1 ; 2; 3] = [2; 4; 6] *)




























let rec sumlist (l: int list) : int =
  match l with
  | [] -> 0
  | h :: t -> h + sumlist t

let rec multlist (l: int list) : int =
  match l with
  | [] -> 1
  | h :: t -> h * multlist t

let rec all_true (l : bool list) : bool = 
  match l with 
  | [] -> true
  | h :: t -> h && (all_true t)

let rec length (l : 'a list) : int = 
  match l with 
  | [] -> 0
  | h :: t -> 1 + length (t) 






let rec fold (f : int -> int -> int) (v : int) (l : int list) : int = 
  match l with 
  | [] -> v
  | h :: t -> f h (fold f v t)

(* Consider f = + and v = 0, then you get sumlist.*)









let rec foldr (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: tail -> f h (foldr f v tail)


(* Consider for example, length of a list. 

let rec length (f: 'a -> int -> int) (v: int) (l : 'a list) : int = 
  match l with 
  | [] -> 0
  | h :: t -> 1 + length t

length l = foldr (fun h foldr_applied_on_tail -> 1 + foldr_applied_on_tail) 0 l  

*)


let rec sumlist (l: int list) : int =
  match l with
  | [] -> 0
  | h :: t -> h + sumlist t

let sumlist l = foldr (fun x y -> x + y) 0 l

(* sumlist [1; 3; 6] 
  = foldr (fun x y -> x + y) 0 [1; 3; 6]
  = 1 + (foldr (fun x y -> x + y) 0 [3; 6])
  = 1 + (3 + (foldr (fun x y -> x + y) 0 [6]))
  = 1 + (3 + (6 + (foldr (fun x y -> x + y) 0 [])))
  = 1 + (3 + (6 + (0)))
  = 10
 
 *)






let rec multlist (l: int list) : int =
  match l with
  | [] -> 1
  | h :: t -> h * multlist t



let multlist l = foldr (fun x y -> x * y) 1 l 



let rec all_true (l : bool list) : bool = 
  match l with 
  | [] -> true
  | h :: t -> h && (all_true t)




let all_true (l : bool list) : bool = foldr (fun x y -> x && y) true l

let length (l : 'a list) : int = foldr (fun x y -> 1 +  y) 0 l






















(* Exercise: What does this do? *)
let funkyfold f l = foldr (fun x y -> (f x) :: y) l












let rec foldr (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: tail -> f h (foldr f v tail)

let temp = foldr (fun (x: int) (y: int) -> x - y) 0 [4; -2; 1; 0]

(* What does the above expression evaluate to?

  1. (4 - (- 2 - (1 - (0 - 0))))
  2. (((4 - (-2)) - 1) - 0) - 0
*)










let rec factorial (n: int): int =
  match n with
  | 0 -> 1
  | _ when n < 0 -> failwith "usual"
  | n -> n * factorial_match (n - 1)

let factorial_fold (n: int): int = foldr ( * ) 1 (range 1 n)



let max_elem (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> Some (foldr max h t)


(* 
    max_elem [2, 12, 1]
  = Some (foldr max 2 [12, 1])
  = Some (max 2 (foldr max 12 [1]))
  = Some (max 2 (max 12 (foldr max 1 [])))
  = Some (max 2 (max 12 1))
  = Some (max 2 12)
  = Some 12
  *)















(* Testing Euclid's Proof *)


let prime_factor (n : int) : int = 
  match (filter (fun x -> (n mod x = 0)) (range 2 (n -1))) with 
  | [] -> failwith "invalid input"
  | h :: _ -> h 


let euclid_tester1 (n : int) : int = 
  range 2 n 
  |> sieve 
  |> foldr ( * ) 1
  |> + 1
  |> prime_factor



let euclid_tester1 (n : int) : int = 
  prime_factor ((foldr ( * ) 1 (sieve (range 2 n))) + 1)














(* Let us consider the problem of finding all find all pythagorean triples under 20

We first write it down mathematically: 

  { (a, b, c) | a in [1; ...; 20], b in [1; ...; 20], c in [1; ...; 20] and a^2 + b^2 = c^2 }

  To construct this, we need: 

  all_triples_under_20 = [(1,1,1), (1, 1, 2), ... (20, 20, 20)]

  and 

  is_pythagorean (a, b, c) = (a^2 + b^2 = c^2)

  Then the solution is: filter is_pythagorean all_triples_under_20

  And we are done!
*)






(* Let us first generate all pair under n *)

(* All pairs under n = 
    [1, 2, ... n]

    And then map each x in this list to 
    x, y 
    
    Where y also comes from [1, 2, ... n].


    {(x, y) : x in [1, ... n], y in [1, ... n] }
    *)



let cart_prod (l1 : int list) (l2: int list) : (int * int) list = 
  match l1 with 
  | [] -> []
  | h :: t -> (map (fun x -> (h, x) l2)) @ (all_pairs t l2)


let cart_prod (l1 : int list) (l2: int list) : (int * int) list = 
  match l1 with 
  | [] -> []
  | h :: t ->
    l2 
    |> map (fun x -> (h, x)
    |> (all_pairs t l2) @





let all_pairs_under (n : int) : (int * int) list =
  range 1 n
  |> map (fun x -> map (fun y -> (x, y)) (range 1 n))
  |> foldr ( @ ) []













let concat (xss : 'a list list) : 'a list =
    foldr ( @ ) xss []


















let all_triples_under n : (int * int * int) list =
  range 1 n
  |> map (fun x -> map (fun (y, z) -> (x, y, z)) all_pairs_under n)
  |> concat












let all_pythagorean_triples n : (int * int * int) list =
  filter (fun (a, b, c) -> a * a + b * b = c * c) (all_triples_under n)
