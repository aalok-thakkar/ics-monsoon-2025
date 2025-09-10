(* Lecture 4: Exponentiation is Just Repeated Multiplication *)

type nat = Zero | Succ of nat

let rec plus (k : nat) (n : nat) : nat = 
  match k with 
  | Zero -> n
  | Succ k2 -> Succ (plus k2 n)

let rec mult (k : nat) (n : nat) : nat = 
  match k with
  | Zero -> Zero
  | Succ k2 -> plus (mult k2 n) n

(* How would you write exponentiation? *)

let rec exp (base: nat) (e: nat) : nat = 
  match e with 
  | Zero -> Succ (Zero)
  | Succ e2 -> mult (base) (exp base e2)


















(* Can we do better? What is better? *)


(* 
- Time efficient
- Space efficient
- Expand the scope
- Error handling
- 
*)















(* Let us go back to binomial coefficients *)

let rec fact (n : int) : int =
(* Requires n >= 0 and n <= 20 *)
(* Ensures fact n = n! if n <= 20 *)
  match n with
  | 0 -> 1
  | n when n < 0 -> raise (Invalid_argument "factorial requires n >= 0")
  | _ -> n * fact (n-1)

let choose (n : int) (k : int) : int =
(* Requires n >= 0 and k >= 0 and k <= n *)
(* Ensures choose n k = n! / (k! * (n-k)!) if requirements met *)
  if n < 0 || k < 0 then
    raise (Invalid_argument "choose_recursive requires n >= 0 and k >= 0")
  else if k > n then
    raise (Invalid_argument "choose_recursive requires k <= n")
  else
    fact n / (fact k * fact (n-k))

let rec choose_recursive (n : int) (k : int) : int =
(* Requires n >= 0 and k >= 0 and k <= n *)
(* Ensures choose_recursive n k = n! / (k! * (n-k)!) *)
  if n < 0 || k < 0 then
    raise (Invalid_argument "choose_recursive requires n >= 0 and k >= 0")
  else if k > n then
    raise (Invalid_argument "choose_recursive requires k <= n")
  else
    match (k, n) with
    | (0, _) -> 1
    | (k, n) when k = n -> 1
    | (k, n) -> choose_recursive (n-1) (k-1) + choose_recursive (n-1) k



















let time_function (f : int -> int -> int) (x : int) (y : int) : float =
  let start_time = Sys.time () in
  let _ = f x y in
  let end_time = Sys.time () in
  end_time -. start_time

let test_binomial_function (f : int -> int -> int) (n : int) (k : int) : unit =
  try
    let v = f n k in
    let t = time_function f n k in
    Printf.printf "value: %d\n" v;
    Printf.printf "runtime: %f seconds\n\n" t
  with Invalid_argument msg ->
    Printf.printf "Error: %s\n\n" msg

let () =
  Printf.printf "Testing choose:\n";
  test_binomial_function choose 19 7;
  Printf.printf "Testing choose_recursive:\n";
  test_binomial_function choose_recursive 19 7

(* How do you measure which one is faster and which one is slower? *)
















(* Big O Notation 

Big O notation describes the upper bound of the growth rate of an algorithm's 
time or space complexity. It tells us how the runtime or memory usage grows as 
the input size increases. 

Formally, we say that a function f(n) is O(g(n)) if there exist positive 
constants c and n0 such that:

   0 ≤ f(n) ≤ c * g(n) for all n ≥ n0

This means that f(n) is eventually bounded above by some constant multiple 
of g(n). The function g(n) gives us a rough approximation of how f(n) grows
as n gets very large.


















Examples from our code:

   choose: 
   - Uses factorial formula directly
   - Constant number of operations regardless of input size
   - But limited by integer overflow for large n
   
   
   
   
   
   Multiply numbers for factorial. 

     Time taken for choose 
   = Time taken for factorial + Time taken for mult and division
   = O(n) + (k) + (n - k) + O(1)
   = O(n + k + (n - k) + 1)
   = O(2n + 1) 
   = O(n)








   choose_recursive Uses recursive tree
   - Each call spawns two more recursive calls
   - Number of calls grows exponentially with n
   - Much slower for large inputs


  Each call for choose_recursive leads to two calls for choose_recursive.






  
  
  In the worst case, we may compute with k = n/2. 

  What is the upper bound on choose_recursive n (n/2)?

  One bound for choose(n, n/2) is 2^n. 









  By the binomial theorem, sum of all choose(n, k) for k from 0 to n is 2^n.

  Therefore, choose(n, n/2) is at most 2^n.

  Additionally, it is the largest term in the sum. And there are n+1 terms.

  Therefore, choose(n, n/2) is at least (2^n)/(n+1).





   This explains the runtime difference we see in our timing tests:
*)








(* The same Pingala who gave us a recursive solution for the binomial 
coefficient problem, also gave us a recursive solution for fast exponentiation. 

For simplicity, let us first define it for ints.

*)


let rec exp (base : int) (power : int) : int =
  match power with
  | 0 -> 1
  | n when n < 0 -> raise (Invalid_argument "exponent must be non-negative")
  | n -> base * exp base (n-1)





(* Pingala's insight was that we can compute powers much faster by using 
   the following observations:

   For even powers: base^n = (base^(n/2))^2
   For odd powers:  base^n = base * (base^((n-1)/2))^2

   This means we can reduce the problem size by half at each step,
   rather than decrementing by 1 each time. For example:

   2^8 = (2^4)^2 = ((2^2)^2)^2 = (((2^1)^2)^2)^2

   This leads to only three squaring steps instead of 7 multiplications. 
*)




let rec pingala_exp (base : int) (power : int) : int =
  match power with
  | 0 -> 1
  | n when n < 0 -> raise (Invalid_argument "exponent must be non-negative")
  | n -> 
      let half = pingala_exp base (n/2) in
      match n mod 2 with
      | 0 -> half * half
      | _ -> base * half * half



(* Now we must ask the following questions:
   1. Correctness: Does Pingala's method produce the same results as before? 
   2. Termination: Will Pingala's method always terminate for valid inputs?
   3. Efficiency: What is the time complexity of each approach?
*)






(* Case Study V: Apple's SSL/TLS "goto fail" Bug (2014)

   A common error in recursive exponentiation is forgetting the base case for 
   power = 1. This can lead to infinite recursion since power/2 rounds down to 
   0, causing the base case to be missed. We avoid this by using 0 as the base 
   case. Missing base cases have caused real-world issues. 

   In 2014, a duplicated goto statement in Apple's SSL/TLS implementation 
   caused certificate validation to be bypassed. The bug looked like:

     if ((err = SSLHashSHA1.update(&hashCtx, &signedParams)) != 0)
         goto fail;
         goto fail;                 // Duplicate line caused unconditional jump

   This emphasizes the importance of careful implementation and testing of 
   security-critical code, even for seemingly simple control flow.

*)




(* How do we prove correctness? *)


(* In order to prove correctness we need a specification of the function. *)









(* What is the specification for exponentiation? *)


(* Requires: power: int, power >= 0 

   Ensures: pingala_exp base power = exp base power

*)


(* 

Theorem: Fast exponentiation (pingala_exp) is correct if for all power : int, 

  when power >= 0, then pingala_exp base power = exp base power.


  Proof: 
    Base Case: 
    power = 0. This is satisfied by definition.

    Induction Hypothesis: 
        Till some power = k, for all j < k, 
              pingala_exp base j = exp base j.

    Induction Step:
         Then for power = k + 1... 

         Case 1: k + 1 is odd.

            pingala_exp base (k + 1) = base * half * half

            where half = pingala_exp base (k/2)
                       = exp base (k/2)           (as k/2 < k, and IH)

            So pingala_exp base (k + 1) = 
                                 base * exp base (k/2) * exp base (k/2)
                                 = exp base (1 + k/2 + k/2)
                                 = exp base (k + 1)

                                 Which is what we wanted to show.


         Case 2: k + 1 is even. 

             Same thing...



*)









(* What about termination? *)


(* At each step, we are halving.
   
   In even case, 

     power > power/2 so we are decreasing

  In odd case, 
     power > (power - 1)/2 so we are decreasing

  Eventually hit base case of power = 0.


*)








(* Let's compare their performance: *)

let () =
  Printf.printf "exp (simple recursive):\n";
  let t1 = time_function exp 3 500 in
  Printf.printf "runtime: %f seconds\n\n" t1;
  Printf.printf "pingala_exp (Pingala's method):\n";
  let t2 = time_function pingala_exp 3 500 in
  Printf.printf "runtime: %f seconds\n\n" t2

(* The simple recursive version is O(n) as it makes n recursive calls.
   Pingala's method is O(log n) as it halves the power each time. *)







(* Case Study VI: Java's Math.pow Integer Inefficiency (Pre-Java 15)

   Before Java 15, Math.pow() always used floating-point arithmetic even for 
   integer inputs. This was inefficient for integer exponentiation since:
   - It required conversions between int and double
   - Used slower floating-point operations
   - Could lose precision for large integers
   
   This inefficiency impacted performance in numerical computing applications
   and cryptography implementations that relied heavily on integer 
   exponentiation. Java 15 added specialized integer methods 
   (Math.multiplyExact etc.) to handle this case more efficiently, similar to 
   our direct integer implementation. This change provided up to 100x speedup 
   for integer exponentiation operations in performance-critical code.
 *)







 

(* 

Proof that pingala_exp is O(log n):

   Let T(n) be the number of recursive calls made by pingala_exp for power n.
   
   For even n:
   T(n) = 1 + T(n/2)    [1 call plus recursion on n/2]
   
   For odd n: 
   T(n) = 1 + T((n-1)/2) [1 call plus recursion on (n-1)/2]
   
   In both cases, the next recursive call operates on roughly n/2.
   
   Starting with n, the sequence of values is approximately:
   n -> n/2 -> n/4 -> n/8 -> ... -> 1
   
   The number of steps to reach 1 is log₂(n).
   
   Therefore T(n) = O(log n).
   
   This matches our empirical timing results showing pingala_exp is much 
   faster than the O(n) simple recursive version for large powers. 
   
*)