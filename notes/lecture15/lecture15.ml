
(* Lecture 15 *)






(* Search for a target element *)



let search (l : 'a list) (x : 'a ) : bool = 
  let found = ref false in 
  (* let t = ref [] *)
  let w = ref l in 
  while (w <> []) do 
    (* Loop Invariant: 
       (i) l = !t @ !w
       (ii) found = true -> x in t. found = false -> x not in t 

       This uses t as a ghost variable.
    *)
    match w with 
    | [] -> failwith "unreachable line"
    | h :: w' -> 
      (if h = x then found := true) ; 
      (* t := t @ [h]*) 
      w := w' 
    done
  !found 
  ;;


let search (l : 'a list) (x : 'a ) : bool = 
  let found = ref false in 
  let w = ref l in 
  while (w <> []) do 
    (* Loop Invariant: 
       (i) !w is a suffix of l
       (ii) found = true -> x in l. 
       (iii) found = false -> x either in w or x not in l
    *)
    match w with 
    | [] -> failwith "unreachable line"
    | h :: w' -> 
      (if h = x then found := true) ; 
      w := w' 
    done
  !found 
  ;;

let count (l : 'a list) (x : 'a ) : int = 
  let counter = ref 0 in 
  let w = ref l in 
  while (w <> []) do 
    (* Loop Invariant: 
       (i) !w is a suffix of l
       (ii) !counter + (count !w x) = (count l x)
    *)
    match w with 
    | [] -> failwith "unreachable line"
    | h :: w' -> 
      (if h = x then counter := !counter + 1) ; 
      w := w' 
    done
  !counter 
  ;;



let find_first_index (l : 'a list) (x : 'a ) : int = 
  let found = ref false in 
  let index = ref -1 in 
  let w = ref l in 
  while (w <> [] && found = false) do 
    (* Loop Invariant: 
       (i) !w is a suffix of l
       (ii) lenght(w) + index = length (l) - 1
       (iii) found = true -> x in l. 
       (iv) found = false -> x either in w or x not in l
    *)
    match w with 
    | [] -> failwith "unreachable line"
    | h :: w' -> 
      (if h = x then found := true) ; 
      w := w'; index := !index + 1;  
    done
  !index 
  ;;
































(* Is Prime? *)


(* 
  Requires: n > 1
  Ensures: 
      For all d in [2, ... , (n - 1)] d does not divide n -> is_prime n = true
      There exists d in [2, ... , (n - 1)] that divides n -> is_prime n = true

*)












let is_prime (n: int) : bool =
  if n < 2 then false
  else
    let d = ref 2 in
    let found_divisor = ref false in
    while (!d <= (n - 1)) && not !found_divisor do
      if n mod !d = 0 then found_divisor := true;
      d := !d + 1
    done;
    not !found_divisor
;;








(* Try to count the number of steps in Collatz Conjecture *)

let rec collatz (n: int) : int =
  if n = 1 then 0
  else if n mod 2 = 0 then 1 + collatz (n / 2)
  else 1 + collatz (3 * n + 1)
;;

let rec collatz_helper (a: int) (n: int) : int =
  if n = 1 then a
  else if n mod 2 = 0 then collatz_helper (a + 1) (n / 2)
  else collatz_helper (a + 1) (3 * n + 1)
;;







(* While Loop? 

let collatz (n : int) : int = 
  let a = ref 0 in 
  let m = ref n in 
  while (!m <> 1) do
    a := !a + 1; 
    if m mod 2 = 0 then 
      m := m / 2;
    else
      m := (3 * !m + 1)
  done;
  !a

*)








let collatz_loop (n: int) : int =
  let num = ref n in
  let steps = ref 0 in
  while !num <> 1 do
    (* Invariant: 
        !steps = number of collatz steps required to get from n to !num *)
    assert(!steps = collatz(n) - collatz(!num)); 

    if !num mod 2 = 0 then
      num := !num / 2
    else
      num := 3 * !num + 1;
    steps := !steps + 1
  done;

  (* From loop condition: !num = 1 *)
  (* From loop invariant: !steps = number of collatz steps required to get from n to !num *)

  (* Therefore, !steps = number of collatz steps required to get from n to 1. *)

  assert(!steps = collatz(n)); 

  !steps
;;



(* How many steps? Does it terminate? *)















let gcd (a : int) (b : int) : int =
  let x = ref a in
  let y = ref b in
  while !y <> 0 do
    (* Progress measure: |b| - |y| *)
    let temp = !y in
    y := !x mod !y;
    x := temp
  done;
  !x
;;

(* 

while loop terminates in :

  1 -> (1, 1)
  2 -> (3, 2)
  3 -> (5, 3)
  4 -> (7, 4)
  5 -> ... 

  k -> (fib_k, fib_{k - 1})

  gcd 7 4 = gcd 4 3 = gcd 3 1 = 1
*)



(* Progress Measure: 

  Parameter 
    (i)   that is initialised to 0.
    (ii)  increases by a non-zero quantity that is bounded below in each iteration
    (iii) is bounded above. 
*)

(* Alternatively, a measure of termination is:

  Parameter 
    (i)   that is initialised to finite quantity.
    (ii)  decreases by a non-zero quantity that is bounded below in each iteration
    (iii) is bounded below.
*)










(* Divide and average for square roots *)








(* Exercise: What is the progress measure for this while loop? *)

let sqrt_da (x : float) : float =
  let guess = ref (x /. 2.0) in
  while (abs_float (!guess *. !guess -. x) > 0.0001) do
    guess := 0.5 *. (!guess +. x /. !guess)
  done;
  !guess
;;













let random_walk (start : int) : unit =
  let pos = ref start in
  while !pos <> 0 do
    print_int !pos; print_newline ();
    if Random.bool () then
      incr pos
    else
      decr pos
  done;
  print_endline "Reached zero!"
;;
