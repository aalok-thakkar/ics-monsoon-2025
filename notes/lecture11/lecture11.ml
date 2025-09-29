(* Lecture 11: Origami Programming *)

let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | h :: t ->
    match pred h with
    | true -> h :: filter pred t
    | false -> filter pred t

let rec range (a: int) (b: int) : int list =
  match a > b with
  | true -> []
  | false -> a :: range (a + 1) b

let rec foldr (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: tail -> f h (foldr f v tail)








let length (l: 'a list) = foldr (fun x y -> 1 + y) 0 l


let funkyfold (f : 'a -> 'b) (l : 'a list) : 'b list = 
  foldr (fun (x: 'a) (y : 'b list) -> f (x) :: y) [] l


let funkyfold2 (p: 'a -> bool) (l: 'a list) : 'a list = 
  foldr (fun x y -> match (p x) with true -> x :: y | false -> y) [] l










(* Left Fold *)


  

(* foldl f v [a1; a2; a3] = 
  match l with 
  | [] -> []
  | h :: t -> foldl f (f v a1) [a2; a3]



*)










let rec foldl (f: 'b -> 'a -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: t -> foldl f (f v h) t


let sumlist_l (l : int list) = foldl (+) 0 l

(* 

  sumlist_l [1; 4; 34; 12]
  = foldl (+) 0 [1; 4; 34; 12]
  = foldl (+) (0 + 1) [4; 34; 12]
  = foldl (+) ((0 + 1) + 4) [34; 12]
  = foldl (+) (((0 + 1) + 4) + 34) [12]
  = foldl (+) ((((0 + 1) + 4) + 34) + 12) []
  = foldl (+) ((((0 + 1) + 4) + 34) + 12) []
  = ((((0 + 1) + 4) + 34) + 12)

*)





(* Try subtraction on foldl and foldr and see they are different *)



(* Write reverse using foldl *)

let reverse (l : 'a list) : 'a list = 
  foldl (fun x y -> y :: x) [] l

(* 

   reverse l v
 = reverse [a1; a2; a3] []
 = reverse [a2; a3] [a1]
 = reverse [a3] [a2; a1]
 = reverse [] [a3; a2; a1]
 = [a3; a2; a1]

 The function f is that which takes in 
 h from l = h :: t
 and v and gives us (f v h)

 f v h -> h :: v


 foldl (fun v h -> h :: v) [] l 


*)









(*  reverse [1; 2; 3] 
  = foldl f [] [1; 2; 3]
  = foldl f [1] [2; 3]
  = foldl f [2; 1] [3]
  = foldl f [3; 2; 1] []
  = [3; 2; 1]
*)



(* Exercise: Can you write reverse with foldr? *)


















 




(* Origami Programming 

The Fun of Programming (Jeremy Gibbons and Oege de Moor), 2003
*)



















(* The n-queens problem *)
(* Max Bezzel published the eight queens puzzle in 1848. Franz Nauck published 
the first solution in 1850 and extended the puzzle to the n queens problem. 
Since then, many mathematicians, including Carl Friedrich Gauss, have worked it. 

Consider a 4x4 chess board. Can you place four queens that do not threaten each 
other? 

        - Q - - 
        - - - Q 
        Q - - - 
        - - Q - 
*)












(* Given n, can you find a nxn chess board configuration where n queens do not threaten each other? How do you even represent such a chessboard? *)

(* As a chess board with "-" and "Q" *)
let representation1 = [
  ["-"; "Q"; "-"; "-"]; 
  ["-"; "-"; "-"; "Q"]; 
  ["Q"; "-"; "-"; "-"]; 
  ["-"; "-"; "Q"; "-"]
] 

(* As just booleans *)
let representation2 = [
  [false; true; false; false]; 
  [false; false; false; true]; 
  [true; false; false; false]; 
  [false; false; true; false]
]

(* As a list of positions of the queens, where index is the row. *)
let representation3 = [
  1; 
  3; 
  0; 
  2
] 

(* As pairs (col, row). We will use this going ahead. *)
let representation4 = [
  (1, 0); 
  (3, 1); 
  (0, 2); 
  (2, 3)
]


(* 
  "The Analytical Engine has no pretensions whatever to originate anything. It 
  can do whatever we know how to order it to perform. It can follow analysis; 
  but it has no power of anticipating any analytical relations or truths. Its 
  province is to assist us in making available what we are already acquainted 
  with."
   -- Ada Lovelace

   In programming, we can represent the same concept in many different ways, 
   just as Ada Lovelace described the flexibility of computation.
*)





(* Check if two queens threaten each other *)
let threatens ((col1, row1) : int * int) ((col2, row2) : int * int) : bool =
  col1 = col2 || row1 = row2 || abs (col1 - col2) = abs (row1 - row2)
  
let forall (p : 'a -> bool) (l : 'a list) = 
  foldr (fun (x : 'a) (y: bool) -> p (x) && y) true l



(* 

        - Q - - 
        - - - Q 
        Q - - - 
        - - - - 

*)




(* Check if a new queen is safe with respect to existing queens *)
let is_safe (new_queen : int * int) (existing_queens : (int * int) list) : bool =
  forall (fun q -> not (threatens new_queen q)) existing_queens 
  







(* Helper function: Generate all possible positions for the next row *)
let next_row (existing_queens: (int * int) list) (n: int) (r: int) : (int * int) list  =
  range 0 (n - 1)
  |> map (fun x -> (x, r)) (* make a list of all positions *)
  |> filter (fun q -> is_safe q existing_queens)      (* and then you filter *)


(* Solve the n-Queens problem starting with a given row *)
let rec solve (n: int) (r: int) (existing_queens: (int * int) list) : ((int * int) list) list =
  match r with
  | _ when r = n -> [existing_queens]
  | _ ->
    next_row existing_queens n r
    |> map (fun r -> (r :: existing_queens))
    |> foldr (fun qs s -> (solve n (r + 1) qs @ s)) []

  
let n_queens n = solve n 0 [] 

(* Representation of a chessboard as a list of lists of strings *)

let chessboard_row (c : int) (n : int) : string = 
  range 0 (n - 1) 
  |> map (fun x -> if (x = c) then "Q" else "-")
  |> foldr (fun x s -> x ^ " " ^ s) ""


let chessboard (queens: (int * int) list) : string = 
  queens
  |> map (fun (c, r) -> (chessboard_row c (length queens)))
  |> foldr (fun x s -> x ^ "\n" ^ s) ""


(* And now finally: pretty-print all solutions *)
let rec print_solutions solutions =
  match solutions with
  | [] -> ()
  | x :: xs ->
      print_endline (chessboard x);
      print_endline "----------";
      print_solutions xs


let () = print_solutions (n_queens 12)








