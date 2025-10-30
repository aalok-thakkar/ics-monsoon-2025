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


(* Given a number n >= 0, find an a such that: 

  a^2 <= n < (a + 1)^2  
  
*)


(* Requires: n >= 0
   Ensures: 
   (i) a^2 <= n
   (ii) n < (a+1)^2
*)


(* Case I: 
    Loop Condition:    a^2 > n
    Loop Invariant:    n < (a+1)^2
*)


(* 

a = ref (n + 1)

while ((!a)^ > n) do  
      a := !a - 1
done
!a
      
*)



(* Case II: 
    Loop Condition:    n >= (a+1)^2
    Loop Invariant:    a^2 <= n
*)


(* 

a = ref 0

while (n >= (a+1)^2) do 
  a := !a + 1
done
!a

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
  m = max l

   Requires: l <> []
   Ensures: 
    (i) for all x in l, m >= x
    (ii) m in l
*)

(* Example: [1; 31; 3]


Let's first check if list is empty. 
  If empty, then error. 
Else: 
  We know that there is a head and tail such that l = head :: tail

   (current_max, rest)
   (1; [31; 3])
   (31; [3])
   (31; []) 

   Return 31
*)





let max (l : int list) : int =
  match l with 
  | [] -> failwith "max: input list is empty"
  | h :: t -> 
    let current_max = ref h in 
    let rest = ref t in
    (* Loop invariant: current_max in l 
                       for all x in l, not in rest, 
                       current_max >= x 
    *)
    while !rest <> [] do
      match !rest with
      | (h :: t') ->
        if h > !current_max then 
          current_max := h; 
        rest := t'
      | _ -> failwith "max: unreachable code"
  done;
  !current_max
;;

(* Proof of Correctness: 


  Loop Invariant: current_max in l and for all x in l, not in rest, current_max >= x 

  Initialisation: 
    l = h :: t
    current_max = h
    rest = t

    For this, 
     current_max = h, hence in l
     The only x in l, x not in rest is the head h. And current_max >= h.

  Maintenance: 
    Before the loop body: 

    current_max = m
    rest = h :: t

    update: 
      if h > m 
        current_max = h
      
      rest = t

    After the loop body: 
    is current_max still in l? 
    Case I: h > m. In that case, we updated it by an element of rest. Rest is a sublist of l. Hence true. 
    Case II: not (h > m). In that case, we did not update current_max. Therefore, by loop invariant, it is true.

    For x in l, x not in rest,  current_max >= x?
    Then either x in l, and x not in rest before the loop, in which case... loop invariant.
    Or x in l, and x = h, then current_max >= h by update, we have this property.

  Termination:
current_max in l and for all x in l, not in rest, current_max >= x 
But rest = [] by loop condition. 

Hence (i) current_max in l 
      (ii) for all x in l, current_max >= x 
*)

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


(* 
   isprime (n : int) : bool ...

   Requires: n > 1
   Ensures: 

   isprime(n) = false if
    there is d <> 1, d <> n, n mod d = 0
   isprime(n) = true otherwise.
   
   We can keep track of the largest number that divides n. 
   Let d range from 2 to (n - 1)
   We will check if d divides n. 
*)


(* 

let isprime (n : int) : bool = 
  let d = ref 2 in
  let ans = ref false in 
  while ((!d < n) && (ans = false)) do 
    match (n mod !d) with 
    | 0 -> ()
    | _ -> ans := ans || true; 
    d := !d + 1    
  done
  ans


  Loop Invariant: There is no factor of n between 2 and d. 
*)















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
