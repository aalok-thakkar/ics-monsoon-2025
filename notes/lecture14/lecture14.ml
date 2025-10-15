(* More on the While Loop *)

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



(* Back to division *)


type nat = Zero | Succ of nat 

(* Define a function div such that it takes two natural numbers a and b, and 
returns the quotient and the remainder when a is divided by b. *)


(* Requires: b <> Zero *)
(* Ensures:

      Let's say div a b = (q, r)

      (i) a = (b * q) + r
      (ii) r < b
*)


















let minus (n : nat) (m : nat) : nat =
  let n_ref = ref n in
  let m_ref = ref m in
  (*
    Loop invariant:
      - n_ref = n - (initial m - current m_ref), i.e., n minus how much we've subtracted so far.
      - m_ref is the remainder of m decrement.
      - At each iteration, (n_ref, m_ref) = (n - k, m - k) for loop counter k
      - Requires: n >= m at the start, so n_ref never < m_ref during the process. Raises if violated.
   *)
  while (!m_ref <> Zero) do
    (* Invariant: !n_ref - !m_ref = n - m, !n_ref >= 0 *)
    if (!n_ref = Zero) then
      failwith "minus : function requires n >= m"
    else
      match !n_ref, !m_ref with
      | Succ n', Succ m' ->
          n_ref := n';
          m_ref := m'
      | _ -> failwith "minus : unreachable"
  done;
  !n_ref
;;


let div (a : nat) (b : nat) : (int * nat) =
  match b with
  | Zero -> failwith "div: Division by zero"
  | _ ->
    let q = ref 0 in
    let r = ref a in
    (*
      Loop invariant:
        - a = b * !q + !r
        - !r < b does not hold until termination
        - !q counts how many times we've subtracted b from a
     *)
    while not (less_than !r b) do
      r := minus !r b;
      q := !q + 1
    done;
    (!q, !r)
;;
















(* The Integer Square Root Question *)


(* Given a number n > 0, find an a such that: 

  a^2 <= n < (a + 1)^2  
  
*)

















(*
  Requires: n > 0
  Ensures: Returns a value a such that a * a <= n < (a + 1) * (a + 1)
*)

let find_integer_sqrt (n : int) : int =
  if n <= 0 then failwith "find_integer_sqrt: n must be > 0";
  let a = ref 0 in
  (* Loop Invariant:
     - 0 <= !a
     - (!a) * (!a) <= n
     - For all k such that 0 <= k <= !a, k * k <= n
     - When loop exits: (!a + 1) * (!a + 1) > n
  *)
  while (!a + 1) * (!a + 1) <= n do
    a := !a + 1
  done;
  !a
;;













(* Find the maximum element in a list *)

















(*
  Requires: true (works for any int list, including empty list)
  Ensures: Returns Some m where m is the largest element if l is non-empty, or None if l is empty
*)
let max (l : int list) : int option =
  let max_val = ref None in
  let rest = ref l in
  while !rest <> [] do
    match (!max_val, !rest) with
    | (Some current_max, h :: t) ->
        if h > current_max then max_val := Some h;
        rest := t
    | (None, h :: t) ->
        max_val := Some h;
        rest := t
    | _ -> failwith "max: unreachable code"
  done;
  !max_val
;;

(* Search for a target element in a list *)












(*
  Requires: true (works for any list and any target value)
  Ensures: returns true if target occurs in l, false otherwise
*)

let search_list (l : 'a list) (target: 'a) : bool =
  let found = ref false in
  let rest = ref l in
  while !rest <> [] && not !found do
    (* Loop invariant: ?? *)
    match !rest with
    | h :: t ->
        if h = target then found := true
        else rest := t
    | [] -> ()
  done;
  !found
;;




(* Is Prime? *)

let is_prime (n: int) : bool =
  if n < 2 then false
  else
    let i = ref 2 in
    let has_divisor = ref false in
    while !i * !i <= n && not !has_divisor do
      if n mod !i = 0 then has_divisor := true;
      i := !i + 1
    done;
    not !has_divisor
;;



(* Try to count the number of steps in Collatz Conjecture *)

let rec collatz (n: int) : int =
  if n = 1 then 0
  else if n mod 2 = 0 then 1 + collatz (n / 2)
  else 1 + collatz (3 * n + 1)
;;








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






(* Nested Loops *)


let print_grid (n: int) (m: int) =
  for i = 1 to n do
    for j = i to 2*i do
      Printf.printf "(%d,%d) " i j
    done;
    print_newline ()
  done
;;


(* Example usage *)
(* print_grid 3 4;; *)
