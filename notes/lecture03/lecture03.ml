exception Not_implemented of string

let is_odd (n : int) : bool = 
  match (n mod 2) with 
  | 0 -> true
  | _ -> false

let collatz (n : int) : int = 
  match (is_odd n) with 
  | true -> 3 * n + 1
  | false -> n / 2

type nat = Zero | Succ of nat

let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 n)

let rec mult (n : nat) (m : nat) : nat = 
  match n with
  | Zero -> Zero
  | Succ n' -> plus (mult n' m) m

let rec mult2 (n : nat) (m : nat) : nat = 
  match n with
  | Zero -> Zero
  | Succ n' -> plus (mult n' m) m

let rec fact (n : int) : int =
  match n with
  | 0 -> 1
  | _ -> n * fact (n - 1)

let rec fact_nat (n : nat) : nat =
  match n with
  | Zero -> Zero
  | Succ n' -> mult (Succ n') (fact_nat n')

let rec fact_int (n : int) : int =
(* Requires n >= 0 *)
(* Ensures fact_int n = n! *)
  match n with
  | 0 -> 1
  | _ -> n * fact_int (n - 1)

let rec safe_fact (n : int) : int =
(* Requires n >= 0 *)
(* Ensures safe_fact n = n! *)
  match n with
  | 0 -> 1
  | n when n < 0 -> raise (Invalid_argument "factorial requires n >= 0")
  | n -> n * safe_fact (n-1)

let safe_mult (a : int) (b : int) : int =
(* Requires true *)
(* Ensures safe_mult a b = a * b or raises overflow exception *)
  match (a, b) with
  | (0, _) | (_, 0) -> 0
  | (a, b) when a > 0 && b > 0 && b > max_int / a -> 
      raise (Invalid_argument "multiplication overflow")
  | (a, b) when a < 0 && b < 0 && b < min_int / a -> 
      raise (Invalid_argument "multiplication overflow")
  | (a, b) when a > 0 && b < 0 && b < min_int / a -> 
      raise (Invalid_argument "multiplication overflow")
  | (a, b) when a < 0 && b > 0 && b > max_int / (-a) -> 
      raise (Invalid_argument "multiplication overflow")
  | (a, b) -> a * b

let rec correct_fact (n : int) : int =
(* Requires n >= 0 *)
(* Ensures correct_fact n = n! *)
  match n with
  | 0 -> 1
  | n when n < 0 -> raise (Invalid_argument "factorial requires n >= 0")
  | n -> safe_mult n (correct_fact (n-1))

let rec triangular (n : int) : int =
(* Requires n >= 0 *)
(* Ensures triangular n = sum of integers from 1 to n *)
  match n with
  | 0 -> 0
  | n when n < 0 -> raise (Invalid_argument "triangular requires n >= 0")
  | n -> n + triangular (n-1)

let triangular' (n : int) : int =
(* Requires n >= 0 *)
(* Ensures triangular' n = n * (n+1) / 2 *)
  n * (n+1) / 2

let rec count_meters (m : int) (k : int) : int =
(* Requires m >= 0 and k >= 0 *)
(* Ensures count_meters m k = number of ways to arrange m laghu and k guru *)
  match (m, k) with
  | (0, 0) -> 1  
  | (m, k) when m < 0 || k < 0 -> 0
  | (m, k) -> count_meters (m-1) k + count_meters m (k-1)

let choose (n : int) (k : int) : int =
(* Requires n >= 0 and k >= 0 *)
(* Ensures choose n k = n! / (k! * (n-k)!) *)
  fact n / (fact k * fact (n-k))

let count_meters_factorial (m : int) (k : int) : int = 
(* Requires m >= 0 and k >= 0 *)
(* Ensures count_meters_factorial m k = choose (m+k) m *)
  choose (m + k) m

let rec choose_recursive (n : int) (k : int) : int =
(* Requires n >= 0 and k >= 0 *)
(* Ensures choose_recursive n k = choose n k *)
  match (k, n) with
  | (0, _) -> 1
  | (k, n) when k = n -> 1
  | (k, n) -> choose_recursive (n-1) (k-1) + choose_recursive (n-1) k

let rec print_row (n : int) (k : int) : unit =
(* Requires n >= 0 and k >= 0 *)
(* Ensures prints binomial coefficients for row n from column k onwards *)
  match (k <= n) with
  | false -> ()
  | true -> 
    Printf.printf "%d " (choose_recursive n k);
    print_row n (k+1)

let rec meru (n : int) (i : int) : unit =
(* Requires n >= 0 and i >= 0 *)
(* Ensures prints Pascal's triangle up to row n *)
  match (i <= n) with
  | false -> ()
  | true -> 
    print_string (String.make (n-i) ' ');
    print_row i 0;
    print_newline ();
    meru n (i+1)


let () = meru 6 0

let pacioli_division (a_score : int) (b_score : int) : float * float =
(* Requires a_score >= 0 and b_score >= 0 *)
(* Ensures proportional division of stakes *)
  let total = float_of_int (a_score + b_score) in
  let a_share = (float_of_int a_score) /. total in
  (a_share, 1.0 -. a_share)

let tartaglia_division (a_score : int) (b_score : int) (game_size : int) : float * float =
(* Requires a_score >= 0 and b_score >= 0 and game_size > 0 *)
(* Ensures proportional division of stakes based on lead *)
     let lead = float_of_int (a_score - b_score) in
     let total = float_of_int game_size in
     let a_share = 0.5 +. (lead /. total) in
     (a_share, 1.0 -. a_share)

let rec sum_choose (m : int) (k : int) : int =
(* Requires m >= 1 and k >= 1 *)
(* Ensures sum_choose m k = sum of choose(total, i) for i from m to total where total = m+k-1 *)
  let total = m + k - 1 in
  match m with
  | m when m > total -> 0
  | m -> (choose_recursive total m) + (sum_choose (m + 1) k)

let pascal_division (a_score : int) (b_score : int) (target : int) : float * float =
(* Requires target > a_score and target > b_score and a_score >= 0 and b_score >= 0 and target > 0 *)
(* Ensures proportional division of stakes based on probability *)
  let m = target - a_score in
  let k = target - b_score in
  let total = m + k - 1 in
  let denominator = float_of_int (int_of_float (2.0 ** float_of_int total)) in
  let a_share = (float_of_int (sum_choose m k)) /. denominator in
  let b_share = 1.0 -. a_share in
  (a_share, b_share)

let rec isqrt_helper (guess : int) (n : int) : int =
(* Requires n >= 0 and guess >= 0 and guess*guess <= n *)
(* Ensures returns r where r*r <= n and (r+1)*(r+1) > n *)
  match (guess * guess <= n) with
  | true -> isqrt_helper (guess + 1) n
  | false -> guess - 1

let isqrt (n : int) : int =
(* Requires n >= 0 *)
(* Ensures returns r where r*r <= n and (r+1)*(r+1) > n *)
  match n with
  | n when n < 0 -> raise (Invalid_argument "square root requires n >= 0")
  | 0 -> 0
  | _ -> isqrt_helper 0 n

let rec div_average_sqrt_helper (n : float) (x : float) : float =
(* Requires n >= 0 and x >= 0 *)
(* Ensures result r approximates √n within floating point precision *)
  match (abs_float (x *. x -. n) < 0.000001) with
  | true -> x
  | false -> div_average_sqrt_helper n ((x +. n /. x) /. 2.0)

let div_average_sqrt (n : int) : float =
(* Requires n >= 0 *)
(* Ensures result r approximates √n within floating point precision *)
  match n with
  | n when n < 0 -> raise (Invalid_argument "square root requires n >= 0")
  | 0 -> 0.0
  | _ -> div_average_sqrt_helper (float_of_int n) 1.0
