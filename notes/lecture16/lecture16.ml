(* Sorting *)


(* What is the blueprint of a sorting algorithm? *)


(* Blueprint: 

sort(l) = l'

Both l and l' are int list

Requires: True
Ensures: (i) Output is in order
        (ii) Output is a permutation of the input.
*)

(* Loop Invariant: suffix is sorted
   Loop Condition: suffix is the full list
*)

(* 

let i = ref 0 in 
while (!i <> n) do 
  insert x in l
  i := !i + 1
done

*)


(* We need a function to insert 

insert (x : int) (l : int list) : int list 

1. Requires: l is sorted
2. Ensures: Output is sorted and contains all elements of l + x. 
*)

let rec insert (x : int) (l : int list) : int list = 
  match l with 
  | [] -> [x]
  | h :: t -> 
    match (x < h) with 
    | true -> x :: l 
    | false -> h :: (insert x t)

(* How to make insert tail recursive? *)


let rec insert_tail_recursive_helper (prefix : int list) (x : int) (suffix : int list) = 
  match suffix with 
  | [] -> prefix @ [x]
  | h :: t -> 
    match (x < h) with 
    | true -> prefix @ (x :: suffix)
    | false -> insert_tail_recursive_helper (prefix @ [h]) x t 

let rec insert_tail_recursive (x : int) (l : int list) : int list= 
  insert_tail_recursive_helper [] x l



(* Writing it as a while loop: *)

let insert_while (x : int) (l : int list) : int list = 
  let prefix = ref [] in 
  let suffix = ref l in 
  let finished = ref false in 

  while (not !finished) do 
    match !suffix with 
    | [] -> (suffix := x :: !suffix; finished := true)
    | h :: t -> 
      (match x < h) with 
      | true -> (suffix := x :: !suffix; finished := true)
      | false -> 
        (
          suffix := t;
          prefix := !prefix @ [h];
        )
  done
  !prefix @ !suffix




let rec insert_sort (l : int list) : int list = 
  match l with 
  | [] -> []
  | h :: t -> insert h (insert_sort t)


let rec insertion_sort_tail_recursive_helper (prefix : int list) (suffix: int list) : int list =
  match prefix with 
  | [] -> suffix
  | h :: t -> insertion_sort_tail_recursive_helper 
                  (t) (insert_tail_recursive h suffix) 


let insertion_sort_tail_recursive (l : int list) : int list = 
  insertion_sort_tail_recursive_helper l []


let insertion_sort_while (l : int list) : int list =
  let prefix = ref l in 
  let suffix = ref [] in 
  while (!prefix <> []) do 
    match !prefix with 
    | [] -> failwith "insert_sort_while: unreachable"
    | h :: t -> 
      prefix := t
      suffix := insert_while h !suffix
  done 
  !suffix
;;



(* Time Complexity

Let T(n) denote time for insertion sort on a list of n elements
Let T'(n) denote time for insert on a list of n elements

T(n) = T'(0) + T'(1) + ... + T'(n - 1)

What is T'(n)? It is n calls to append. Let's assume that takes constant time C. 

Therefore, T'(n) = C.n. It is linear. 

T(n) = C. (0 + 1 + ... (n - 1))
     = O(n^2)

Therefore insertion sort is quadratic. *)





















(* Selection Sort-ish *)



(* 

Requires: l : 'a list (and comparison is defined on 'a, for example int, float, string)
Ensures: sort (l) = l'
      a) l' is a permutation of l
      b) l'[0] <= l'[1] <= ... <= l'[len(l') - 1] 
*)


(* Let us say that the first i elements are the first i elements of the sorted list *)

(* 
All we need to do is, extract the i+1th element. 
That is by computing minimum of the suffix.
*)

let rec min (l : int list) : int = 
  match l with 
  | [] -> failwith "min: empty list"
  | [h] -> h
  | h :: t -> 
    match (h < min t) with 
    | true -> h
    | false -> min t

let min_tail_recursive (l : int list) : int = 
  match l with 
  | [] -> failwith "min_tail_recursive: empty list"
  | h :: t -> min_tail_recursive_helper h t 

let min_tail_recursive_helper (a : int) (l : int list) : int = 
  match l with 
  | [] -> a
  | h :: t -> 
    match (a < h) with 
    | true -> min_tail_recursive_helper a t
    | false -> min_tail_recursive_helper h t

let min_while (l : int list) : int = 
  match l with 
  | [] -> failwith "min_while: empty list"
  | h :: t -> 
    let current_min = ref h in 
    let to_been_seen = ref t in 
    while (to_been_seen <> []) do
      (match to_been_seen with 
      | [] -> failwith "min_while: unreachable code"
      | h :: t -> 
        match (h < current_min) with 
        | true -> (current_min := h)
        | false -> ()
        ) ; 
        to_been_seen := t
    done 
    !current_min


let rec remove (l : int list) (elem : int) : int list = 
  match l with 
  | [] -> failwith "remove: elem not found"
  | h :: t -> 
    match (h = elem) with 
    | true -> t
    | false -> h :: (remove t elem)


let remove_tail_recursive (l : int list) (elem : int) : int list = 
  match l with 
  | [] -> failwith "remove: elem not found"
  | h :: t -> 
    remove_tail_recursive_helper [] elem l 

let rec remove_tail_recursive_helper (prefix : int list) elem : int (suffix : int list) = 
  match suffix with 
  | [] -> prefix 
  | h :: t -> 
    match (h = elem) with 
    | true -> prefix @ t
    | false -> remove_tail_recursive (prefix @ [h]) elem (t)


(* Practice Question for the test: 

      Write one while loop that does the following: 

      min_remove_while (l : int list) : (int * int list)

      1) First entry of output is (min l)
      2) Second entry of output is (remove (min l) l)

      This should be done by iterating over the loop only.
*)

let rec selection_sort (l : int list) : int list = 
  match l with 
  | [] -> []
  | _ -> 
    let m = (min l) in 
    m :: sort (remove l m)


(* Properties: 
    Sort takes in a list and returns an ordered permutation of it. 
    Min takes in a non-empty list and returns the minimal element of it.
    Remove takes an element x and a list l containing x, and returns l' that contains all elements of l and one less instance of x. 
*)









let rec remove_tail_recursive (seen: int list) (unseen : int list) : int list = 
  match unseen with 
  | [] -> failwith "remove: elem not found"
  | h :: t -> 
    match (h = elem) with 
    | true -> seen @ t
    | false -> remove_tail_recursive (h :: seen) (t)





    


let rec max (l : int list) : int = 
  match l with 
  | [] -> failwith "min: empty list"
  | [h] -> h
  | h :: t -> 
    match (h > min t) with 
    | true -> h
    | false -> min t

let rec sort_tail_recursive_helper (l : int list) (a : int list) : int list = 
  match l with 
  | [] -> a
  | _ -> 
    let m = (max l) in sort_tail_recursive_helper (remove l m) (m :: a) 

let sort_tail_recursive (l : int list) : int list = 
  sort_tail_recursive_helper l []






  
let sort_loop (l : int list) : int list = 
  let a = ref [] in 
  let l2 = ref l in
  while (!l2 <> []) do
    let (m, l_removed_max) = max_loop (!l2) in 
    a := m :: !a; 
    l2 := l_removed_max; 
  done; 
  !a 

let max_loop (l : int list) : (int * int list) = 
  let max = ref None in 
  let seen = ref [] in 
  let unseen = ref l in 
  while (!unseen <> []) do 
    match unseen with 
    | [] -> failwith "max_loop: not possible"
    | h :: t -> 
      match !max with 
      | None -> max := Some h;
      | Some m -> 
        if (h > m) then max := Some h;
      seen := h :: !seen;
      unseen := t ;
    done 

  match !max with 
  | None -> failwith "max_loop: list is empty"
  | Some m -> (m, seen) 
;;




