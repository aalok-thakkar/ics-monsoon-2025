(* Lecture 12: Programs as Procedures *)






(* Tail Recursion *)

let rec factorial (n : int) : int = 
  match n with
  | 0 -> 1
  | _ -> n * factorial (n - 1)


(* Let us look at how these functions are implemented by the computer.

When we call factorial 3, we have the following:

factorial 3 -> 
  Not the base case ->
    Write that you need to compute '3 * factorial 2' 
    and then compute 'factorial 2'

This process is called creating a stack frame. Basically, the computer is 
writing down that after the recursive call, it needs to do something else. 
In this case, it needs to multiply by 3. 

That is ...

factorial 3 -> Create a stack frame for 3 * factorial 2
factorial 2 -> Create a stack frame for 2 * factorial 1
factorial 1 -> Create a stack frame for 1 * factorial 0
factorial 0 -> Base case -> Return 1

Then, the process unwinds ... 

factorial 1 = 1 * 1 -> Return 1
factorial 2 = 2 * 1 -> Return 2
factorial 3 = 3 * 2 -> Return 6

When factorial is called, the computer stores all the waiting function calls. 
All the stack frames. This can take a lot of memory, as well as a lot of time. 
One way to reduce it is called tail recursion! *)

(* Tail Recursion *)
(* The recursive call is the last operation in the function *)

let rec factorial_helper (a : int) (n : int) : int =
  match n with
  | 0 -> a
  | _ -> factorial_helper (a * n) (n - 1)


let factorial_tail (n : int) : int = factorial_helper 1 n












(* 

What is happening here? 

factorial_tail 3 -> calls factorial_helper 1 3

factorial_helper 1 3 -> Not the base case -> create stack frame `factorial_helper (1 * 3) 2'
factorial_helper 3 2 -> ...

But hey, I can just reuse the stack frame, as the previous stack frame had only 
a recursion call, no other operations. 

Hence:

factorial_helper 3 2 -> factorial_helper (3 * 2) 1
factorial_helper 6 1 -> factorial_helper (6 * 1) 0
factorial_helper 6 0 -> Base case reached -> returns 6

So we had only one stack frame, reused. At the implementation level, this 
needs low memory. And is faster. 

This optimization is called Tail Call Optimization (TCO).

*)
















(* What about Fibonacci? *)

let rec fib (n : int) : int =
  match n with
  | _ when n < 0 ->  failwith "invalid input" 
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)



let rec fib_helper (a1: int) (a2: int) (a3: int) (n : int) : int = 
  match n with 
  | _ when n < 0 ->  failwith "invalid input" 
  | 0 -> a1 
  | _ -> fib_helper (a2) (a3) (a2 + a3) (n - 1)

let fib_tail_recursive n = fib_helper 1 1 2 (n - 1)


(* Exercise: Can you do the same computation with just two int as memory? *)


let rec map (f: 'a -> 'b) (l : 'a list) : 'b list = 
  match l with 
  | [] -> []
  | h :: t -> (f h) :: (map f t)



let rec map_helper (f: 'a -> 'b) (a: 'b list) (l: 'a list) : 'b list =
  match l with 
  | [] -> a 
  | h :: t -> map_helper f ((f h) :: a) t 

let map_tail_recursive (f: 'a -> 'b) (l: 'a list) : 'b list =
  map_helper f [] l
  


(* What will map_helper (fun x -> x) [] [1; 2; 3] give us? 


   map_helper (fun x -> x) [] [1; 2; 3]
 = map_helper (fun x -> x) [1] [2; 3]
 = map_helper (fun x -> x) [2; 1] [3]
 = map_helper (fun x -> x) [3; 2; 1] []
 = [3; 2; 1]

*)


let rec reverse_helper (a: 'a list) (l: 'a list) : 'a list =
  match l with 
  | [] -> a 
  | h :: t -> reverse_helper (h :: a) t 


let reverse_tail_recursive (l: 'a list) : 'a list =
  reverse_helper [] l
  


(* Back to Map: *)


let rec map_helper2 (f: 'a -> 'b) (a: 'b list) (l: 'a list) : 'b list =
  match l with 
  | [] -> a 
  | h :: t -> map_helper2 f (a @ [f h]) t 


let map_tail_recursive2 (f: 'a -> 'b) (l: 'a list) : 'b list =
  map_helper2 f [] l
  

let map_tail_recursive3 (f: 'a -> 'b) (l: 'a list) : 'b list =
  reverse_tail_recursive (map_helper f [] l)



(* This is based on the principle that the composition of tail recursive functions is tail recursive. *)





(* How to make this tail recursive? *)

let rec gcd (a: int) (b: int) : int = 
  match (a, b) with 
  | (0, _) -> b
  | (_, 0) -> a
  | (a, b) when a < b -> gcd a (b - a)
  | _ -> gcd (a - b) b











(* Is it really faster? We can measure it as follows: *)

let measure_time (f : 'a -> 'b) (arg : 'a) : float =
  let start_time = Sys.time () in
  let _ = f arg in
  Sys.time () -. start_time

(* Measure the time for both the implementations *)
let n = 10

let () = Printf.printf "Testing Fib(%d):\n" n;
  Printf.printf "Tail-Recursive Fibonacci Time: %f seconds\n" (measure_time fib_tail_recursive n);
;;

let () = 
  Printf.printf "Naive Fibonacci Time: %f seconds\n" (measure_time fib n);
;;

  





(* Can map, filter, and fold be converted to tail recursions? *)

let rec foldl (f : 'b -> 'a -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> foldl f (f v x) xs


(* Turns out foldl is already tail recursive. What about foldr? *)

(* THIS DOES NOT WORK:
let rec foldr (f : 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: t -> f h (foldr f v t)
  
*)

(* Claim: Is foldl just the tail recursive version of foldr? *)
(* Is there a tail recursive version of foldr? *)




let rec sumlist (l : int list) : int = 
  match l with 
  | [] -> 0
  | h :: t -> h + (sumlist t) 


let rec sumlist_helper (a: int) (l : int list): int = 
  match l with 
  | [] -> 0
  | h :: t -> sumlist_helper (a + h) t

let sumlist_recursive (l : int list) : int = sumlist_helper 0 l









(* Fold left is tail recursive. Fold right is just fold left in the reverse. 
As the composition of tail recursive functions is tail recursive, all we need 
is a tail recursive list reversal *)


let rec concat (l1 : 'a list) (l2 : 'a list) : 'a list = 
  match l1 with 
  | [] -> l2
  | (h :: t) -> h :: (concat t l2)

(* Exercise: Convert concat into tail recursive function *)

(* Observe concat l1 l2 = foldr (fun x y -> y :: x) l2 l1 *)









(* Can we make all recursive functions tail recursive? *)


















(* Are all recursive functions tail recursive? Clearly not by default. 
Can every recursive function be transformed into a tail-recursive form? 
Think about it. *)













(* Consider this: *)

let rec layer (f : int -> int) (x : int) (n: int) : int = 
  match n with 
  | _ when n < 0 -> failwith "layer: negative n"
  | 0 -> x
  | _ -> f (layer f x (n - 1))

(* This can be made into a tail recursive function without new arguments or helper functions. *)

let rec layer_tail (f : int -> int) (x : int) (n : int) : int = 
  match n with 
  | 0 -> x
  | _ -> layer_tail f (f x) (n - 1)






(* 
  The unit type in OCaml is a special type that has only one value: (). 
  It is used to indicate that a function does not return any meaningful value, 
  but may perform side effects such as printing, modifying a reference, or 
  interacting with the outside world.

  For example:
*)

(* let print_hello = print_endline "Hello, world!" *)
(* The type of print_hello is unit -> unit *)

(* You call it as: 
let () = print_hello
*)

(* Functions returning unit are often used for their side effects. 
  For example, printing all elements of a list: *)

let rec print_list (l : int list) : unit =
  match l with
  | [] -> ()
  | h :: t -> Printf.printf "%d " h; print_list t; 
;;


let () = print_list [1;2;3;4; 45; 45]; print_endline "" ;;


let rec layer_tail (f : int -> unit) (n : int) : unit = 
  match n with 
  | 0 -> ()
  | _ -> (f n) ; layer_tail f (n - 1)
;;

let rec layer_tail2 (f : int -> unit) (n : int) (init : int): unit = 
  match (init < n) with 
  | true -> (f init) ; layer_tail2 f n (init + 1)
  | false -> ()
;;

let rec layer_tail3  (f : int -> unit) (n : int) (init : int): unit = 
  for i = init to n do 
    f (i)
  done 
;;


let () = 
for i = 1 to 5 do
  Printf.printf "i = %d\n" i
done


(* Use this to print numbers 1 to n. *)






(* In summary, use unit when you want to indicate that a function is called for
 its effect, not for its result. This is common in printing, mutation,
 assertion, or other imperative actions.
*)

let rec layer_unit (f : int -> unit) (n : int) : unit =
  match n with
  | 0 -> ();
  | _ -> f n; layer_unit f (n - 1);
;;

(* Here is a concrete example:  *)

(* 
let print_n n = Printf.printf "%d\n" n ;;
let () = layer_unit print_n 4 ;;

*)

(* This prints:

4
3
2
1

How to do it in the other order? *)

let rec layer_unit_asc (f : int -> unit) (n : int) : unit =
  match n with
  | 0 -> ();
  | _ -> layer_unit_asc f (n - 1); f n;
;;

(* This correct. Here, the recursive function layer is called first, and then 
  f is called. However, this is not tail recursive. How can you make it tail 
  recursive? We will use a helper function. Instead of tracking an object 
  representing data, we will track the number of times the recursion has been 
  called. Consider: 
  
  *)

let rec layer_asc_helper (i: int) (f : int -> unit) (n : int) : unit =
  match (i = n) with 
  | true -> f n;
  | false -> f i; layer_asc_helper (i+1) f n


let layer_unit_tail (f : int -> unit) (n : int) : unit = layer_asc_helper 1 f n 

(* Alternatively, using if ... then ... else, and with (i < n), just to cover all cases: *)

let rec layer_asc_helper_ite (i: int) (f : int -> unit) (n : int) : unit =
  if i > n then 
    ()
  else begin
    f i;
    layer_asc_helper_ite (i + 1) f n
  end


(* This is a very specific type of tail recursion. Such a tail recursion is called a for loop. *)

let layer_unit_for (f : int -> unit) (n : int) : unit =
  for i = 1 to n do
    f i
  done









(* Now using this, let us compute factorial. *)

(* Solution: *)

let rec factorial_helper (a : int) (n : int) (i : int): int = 
  match (i > n) with 
  | true -> a
  | false -> factorial_helper (a * i) n (i + 1)


let factorial (n: int) : int =
  (* references *)
  let a = ref 1 in
  for i = 1 to n do
    (* assignments and deref. *)
    a := !a * i
  done;
  !a
;;






(* We need to learn some concepts before we parse this code *)

(* Recall let statements: *)

let x = 5 ;;

(* On the left side you have a variable. On the right side of equality you 
have an expression. Either a primitive expression or something like: *)

let y = x + 4 ;;

(* And of course we have functions like let rec ... or let ...*)

(* In general, a let statement binds an expression with a name. OCaml supports 
nested let bindings and shadowing: *)

let x2 = 5 in
let y2 = x2 + 4 in
y2;;

(* This won't work let z = x2 - 2 ;; *)

(* Shadowing creates a new binding without altering the original value. That 
is why we can have something like: *)

let x = 5 ;;
(* 
Printf.printf "%d \n" x;;*)

let x = x + 1 ;; 
(* New binding that shadows the old one *)

(* Consider the set of all information that defines the condition of a program 
at a particular point in time. We call this snapshot of the program's memory 
and variable values as the program state *)

let x = 5 ;;
let y = x + 2;;

(* Here, after line, the state of the program is (x, y) = (5, 7). *)

(* A mutable reference in OCaml is a way to store a value that can be modified 
after it's created.

Everything you have seen till now has been an immutable variable. That is, 
once you assign a value to a variable, you can't change it. You may create a 
new variable with the same name and overwrite the old variable. 

A mutable reference allows you to work around this restriction by creating a 
container (or cell) that holds a value. This is closer to how computers 
actually store information in memory. *)

let x = ref 5

(* This creates a reference that holds the value 5.
x is not directly the value 5 — it's a container (or pointer) that holds the 
value 5. *)

(* What is the type of this x? *)
(* x : int ref *)

(* To access a value in a reference, we use dereferencing: *)

(* (!) : 'a ref -> 'a  *)
let value = !x ;; 
(* value = 5 *)

(* To update x to something else we write: *)
x := 10 ;; 

(* Changes the value stored in x to 10 *)

(* Let us revisit factorial: *)

let factorial (n: int) : int =
  let a = ref 1 in
  for i = 1 to n do
    a := !a * i
  done;
  !a
;;

(* Line  is the body of the for loop. By using mutable references, we ensure 
that there are no functions. The operations do not produce an output, that is, 
have a unit type. *)

(* How does it connect to tail recursion? *)

let rec factorial_helper (a : int) (i: int) (n : int) : int =
  match (i > n) with
  | true -> a
  | false -> factorial_helper (a * i) (i + 1) n
;;

let factorial_tail (n : int) : int =
  factorial_helper 1 1 n
;;

(* Observe that the state of the program is modified in a similar way in the 
two cases. *)





(* How do you prove the correctness of a program with a for loop? By defining 
properties on the state of the program. *)

(* A loop invariant is a property (or condition) of the state of the program 
that:

1. Holds before the loop starts.
2. Remains true after every iteration of the loop.
3. When the loop ends, it helps prove that the program produces the correct 
result. *)

let factorial (n: int) : int =
  let a = ref 1 in
  (* Loop Invariant: At the start of each iteration !a = factorial(i - 1) *)
  for i = 1 to n do
    (* At the start of iteration i:
       Invariant holds: !a = (i - 1)!
       We are about to compute i-th factorial by multiplying !a by i.
    *)
    a := !a * i;

    (* After this step:
       Invariant holds: !a = factorial(i)
    *)
  done;

  (* When the loop terminates:
     i = n + 1
     Therefore, !a = factorial(n)
  *)
  !a
;;

(* Loop invariants are similar to proofs using the Principle of Mathematical Induction. 

Initialization, that is, the condition must hold before the start of the loop is like the base case. 

The Maintenance Step, that is, if the condition holds before the start of an iteration it must hold at the end of it is like the inductive step.

And finally, the Termination Step completes the proof.

Going forward, every program with a for loop must have a loop invariant. Or rather, first write a loop invariant, and then write a loop. *)


(* Let us write a loop for the following: *)

let rec fib (n : int) : int =
  match (n <= 0) with 
  | true -> 0 
  | false ->
    match (n = 1) with 
    | true -> 1
    | false -> fib (n - 1) + fib (n - 2)
;;

(* 

Requires: n >= 0
Ensures: fib 1 = 1 and fib 0 = 1 and for all n >= 2, fib(n) = fib(n - 1) + fib(n - 2)


I need this claim: 
  For all i in 2 to n, 
        fib (i) = fib(i - 1) + fib (i - 2)


Somehow if I can compute and store fib (n - 1) correctly and fib (n - 2) correctly then I can compute fib n correctly.

Then store fib (n - 1) and also store fib (n - 2). 

Let storage box a store fib (n - 1) and storage box b store fib (n - 2). 


*)

(* Semantics of if-then-else ... *)


let fib_for (n: int) : int =
  if n <= 0 then 0
  else if n = 1 then 1
  else (
    let a = ref 0 in
    let b = ref 1 in 
    for i = 2 to n do
      (* Invariant: At the start of iteration i = k,
         !a = fib(k - 2) and !b = fib(k - 1) *)
      let next = !a + !b in (* next = fib(k) *)
      a := !b; (* a = fib(k - 1) *)
      b := next; (* b = fib(k) *)
      (* Invariant: At the end of iteration i = k,
         !a = fib(k - 1) and !b = fib(k) *)
    done;
    (* Value of i = n + 1 *)
    !b (* b = fib(i - 1) = fib(n) *)
  )
;;







(* How would you do GCD? *)

let rec gcd (a: int) (b: int) : int =
  if b = 0 then a
  else gcd b (a mod b)
;;

(* You do not know how many times to repeat the operation,
   because the number of recursive calls depends on the size of the inputs
   and how quickly they reduce through the modulo operation.
   
   For this, we will use the while loop! *)

let rec while_loop (condition: bool) (body: unit) =
  if condition then (
    body;
    while_loop condition body
  )
;;