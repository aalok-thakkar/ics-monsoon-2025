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

(* Till now we have seen: *)

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



(* Time Complexity: Quadratic. *)


(* Selection Sort *)



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

let min_remove_while (l : int list) : (int * int list) =
  match l with
  | [] -> failwith "min_remove_while: empty list"
  | h :: t ->
      let current_min = ref h in
      let to_be_seen = ref t in

      while !to_be_seen <> [] do
        match !to_be_seen with
        | [] -> ()  (* unreachable *)
        | x :: xs ->
            if x < !current_min then current_min := x;
            to_be_seen := xs
      done;

      (* remove the first occurrence of that min *)
      let result_list = ref [] in
      let found = ref false in
      let remaining = ref l in

      while !remaining <> [] do
        match !remaining with
        | [] -> ()
        | x :: xs ->
            if not !found && x = !current_min then begin
              found := true;
              remaining := xs
            end
            else begin
              result_list := !result_list @ [x];
              remaining := xs
            end
      done;

      (!current_min, !result_list)

let selection_sort_while (l : int list) : int list =
  let unsorted = ref l in
  let sorted = ref [] in

  while !unsorted <> [] do
    let (m, rest) = min_remove_while !unsorted in
    sorted := !sorted @ [m];   (* append min to sorted list *)
    unsorted := rest;          (* continue with remaining elements *)
  done;

  !sorted



(* Merge Sort *)


(* Merge *)

(* Blueprint

merge (left : int list) (right : int list) : int list 

1. Requires: left and right are sorted
2. Ensures: output is sorted and output is a permutation of left @ right.

*)

let rec merge (left : int list) (right : int list) : int list = 
  match left, right with 
  | [], _ -> right
  | _ , [] -> left 
  | h1 :: t1, h2 :: t2 -> 
    match (h1 < h2) with 
    | true -> h1 :: merge t1 right
    | false -> h2 :: merge left t2
  ;;

(* What is the time complexity? *)

(* let |left| = n 
   let |right| = m

   let k = n + m
   
   T_merge(k) <= O(1) + T_merge(k - 1)
   
   
   *)

let rec take (n : int) (l : int list) : int list = 
  match l, n with 
  | [], _ -> []
  | _ , 0 -> []
  | h :: t, _ -> h :: (take (n - 1) t)

let rec drop (n : int) (l : int list) : int list = 
  match l, n with 
  | [], _ -> []
  | _ , 0 -> l
  | h :: t, _ -> drop (n - 1) t




let rec split (l : int list) : int list * int list =
  match l with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | h1 :: h2 :: t ->
      let (left, right) = split t in
      (h1 :: left, h2 :: right)




let rec mergesort (l : int list) : int list =
  match l with
  | [] -> []
  | [_] -> l
  | _ ->
      let (left, right) = split l in
      merge (mergesort left) (mergesort right)


(* Exercise: What if we split the list into k different parts? *)



(* Tail recursive merge? *)



let rec merge_tail (left : int list) (right : int list) (a: int list): int list = 
  match left, right with 
  | [], _ -> a @ right
  | _ , [] -> a @ left
  | h1 :: t1, h2 :: t2 -> 
    match (h1 < h2) with 
    | true -> merge_tail t1 right (a @ [h1])
    | false -> merge_tail left t2 (a @ [h2])
  ;;

let rec split_tail (l : int list) (l_a : int list) (r_a : int list) : int list * int list =
  match l with
  | [] -> (l_a, r_a)
  | [x] -> (x :: l_a, r_a)
  | h1 :: h2 :: t -> split_tail (t) (h1 :: l_a) (h2 :: r_a)


let rec mergesort (l : int list) : int list =
  match l with
  | [] -> []
  | [_] -> l
  | _ ->
      let (left, right) = split l in
      merge (mergesort left) (mergesort right)

