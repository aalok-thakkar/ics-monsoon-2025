(* Tail recursive merge? *)


let rec mergesort (l : int list) : int list = 
  match l with 
  | [] -> []
  | [x] -> l
  | _ -> let (left, right) = split l in 
         merge (mergesort left) (mergesort right)





(* What are we trying to do with this function? 

  Requires: all the sublists are sorted 
  Ensures: Pairs of sublists are merged, and all sublists are sorted. 
*)
let rec merge_pairs (l : int list list) (a : int list list): (l : int list list) =
  match l with 
  | [] -> a
  | [x] -> a @ [x]
  | h1 :: h2 :: t -> merge_pairs (a @ (merge h1 h2)) (t)

let rec merge_pairs_r (l : int list list) : int list = 
  match l with 
  | [] -> []
  | [x] -> x
  | _ -> merge_pairs_r (merge_pairs l [])

let mergesort (l : int list) : int list = 
  l
  |> List.map (fun x -> [x])
  |> merge_pairs_r




let rec merge_tail (left : int list) (right : int list) (a: int list): int list = 
  match left, right with 
  | [], _ -> a @ right
  | _ , [] -> a @ left
  | h1 :: t1, h2 :: t2 -> 
    match (h1 < h2) with 
    | true -> merge_tail t1 right (a @ [h1])
    | false -> merge_tail left t2 (a @ [h2])

let rec merge_pairs (a : int list list) (l : int list list) : int list list =
  match l with
  | h1 :: h2 :: tail -> merge_pairs ((merge_tail h1 h2 []) :: a) tail
  | [x] -> x :: a
  | [] -> a

let rec mergesort_tail (l : int list list) : int list =
  match l with
  | [] -> []
  | [x] -> x
  | _ -> mergesort_tail (merge_pairs [] l)

let mergesort (l : int list) : int list =
  mergesort_tail (List.map (fun x -> [x]) l)











(* Cleaner Way *)


let rec pair_up (l : 'a list list) : ('a list * 'a list) list =
  match l with
  | h1 :: h2 :: rest -> (h1, h2) :: pair_up rest
  | [x] -> [(x, [])]
  | [] -> []


let rec merge_pairs (l : int list list) : int list list =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ -> 
      l
      |> pair_up
      |> List.map (fun (a, b) -> merge_tail a b [])
      |> merge_pairs


let mergesort (l : int list) : int list =
  l 
  |> List.map (fun x -> [x]) 
  |> merge_pairs 
  |> fun l -> match l with h :: _ -> h | [] -> []



(* Worst Way to Write it *)



let rec foo a = function h1::h2::t -> foo ((h1,h2)::a) t | [x] -> (x,[])::a | [] -> a
let rec bar = function [] -> [] | [x] -> [x] | l -> foo [] l |> List.map (fun (a,b) -> merge_tail a b []) |> bar
let mergesort l = l |> List.map (fun x -> [x]) |> bar |> fun l -> match l with h::_ -> h | [] -> []














(* Can we write this as a loop? *)


let mergesort (l : int list) : int list =
  let current_list = ref (List.map (fun x -> [x]) l) in

  while List.length !current_list > 1 do
    current_list := 
      !current_list
      |> pair_up
      |> List.map (fun (a,b) -> merge_tail a b [])
  done;

  match !current_list with
  | h :: _ -> h
  | [] -> []









(* Quick Sort *)












let rec quicksort (l : int list) : int list =
  match l with
  | [] -> [] 
  | [x] -> [x]                                 
  | h :: t ->
      let left  = List.filter (fun x -> x < h) t in
      let right = List.filter (fun x -> x >= h) t in
      quicksort left @ [h] @ quicksort right


(* Tail Recursive Quick Sort is in the Final Exam *)


(* Processed: ...
   Unprocessed: ...

   Remove a sublist from unprocessed, and divide it into left, h, and right.
   Put [h] and right in unprocessed. and recursively process left. 

   If the sublist is empty, there is nothing to process, and therefore you can put it in processed. 
   *)

let rec quicksort_helper (processed : int list) (unprocessed : int list list) : int list = 
  match unprocessed with 
  | [] -> processed
  | [] :: rest -> quicksort_helper processed rest 
  | [x] :: rest -> quicksort_helper (processed @ [x]) rest
  | (h :: t) :: rest -> 
    let left  = List.filter (fun x -> x < h) t in
    let right = List.filter (fun x -> x >= h) t in
    quicksort_helper (processed) (left :: [h] :: right :: rest)

let quicksort (l : int list) : int list =
  quicksort_helper [] [l] 






















let quicksort_loop (l: int list) : int list =
  let processed = ref [] in
  let unprocessed = ref [l] in
  while !unprocessed <> [] do
    match !unprocessed with
    | [] :: rest ->
        unprocessed := rest
    | [x] :: rest ->
        processed := !processed @ [x];
        unprocessed := rest
    | (h :: t) :: rest ->
        let left  = List.filter (fun x -> x < h) t in
        let right = List.filter (fun x -> x >= h) t in
        unprocessed := left :: [h] :: right :: rest
  done;
  !processed


(* Invariants: 

  1. All elements of l are either in processed or unprocessed
  2. Processed is sorted
  3. If li and lj are sublists in unprocessed, 
      li comes before lj if and only if 
        every element of li < every element of lj
 *)





(* Time Complexity *)






(* Best Case? Worst Case? *)










(* What about the Average Case? *)





(* T(n) is expected time to sort n elements. First pivot
chooses ith smallest element, all equally likely. Then:

T(n) = n + 1/n Sum T(i) + T(n - i - 1)
     = n + 2/n Sum T(i)

Let S(n) = Sum T(i)

Then:

n T(n) = n^2 + 2 S(n)
(n - 1) T(n - 1) = (n - 1)^2 + 2 S(n - 1)

And S(n) = S(n - 1) + T(n - 1)

This gives us: 

nT(n)− (n−1)T(n−1) = (n^2−(n−1)^2)+2T(n−1)
nT(n)              = (2n - 1) + (n + 1)T(n - 1)

Let U(n) = T(n)/n + 1.

Then U(n) <= U(n - 1) + 2/n
So U(n) = O(log n), and 
T(n) = O(n log n)

*)