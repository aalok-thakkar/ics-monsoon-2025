let rec quicksort (l : int list) : int list =
  match l with
  | [] -> [] 
  | [x] -> [x]                                 
  | h :: t ->
      let left  = List.filter (fun x -> x < h) t in
      let right = List.filter (fun x -> x >= h) t in
      quicksort left @ [h] @ quicksort right


(* Processed: accumalator
   Unprocessed: stack

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













(* Lower Bound on Sorting *)



(* A comparison-based sorting algorithm learns the order of elements only by asking questions of the form: “Is A < B?” Each such comparison gives one bit of information (yes/no). To sort n elements, the algorithm must figure out which of the n! possible orderings is the correct one.

So after:
    1 comparison  -> 2 possibilities remain
    2 comparisons -> 4 possibilities remain
    k comparisons -> 2^k possibilities remain


Therefore, if the algorithm uses at most k comparisons, it can distinguish at most 2^k different cases. To distinguish n! cases you need at least log(n!) comparisons. 

  What is log (n!)?
*)




(* In-class test on Nov 24 *)
(* Update to the Enigma assignment *)