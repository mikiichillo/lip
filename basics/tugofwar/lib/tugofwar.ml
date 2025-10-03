(* tokens *)
type token = A | B | X

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let toklist_of_string s = 
  let l = explode s in
  List.map(function
    | 'A' -> A
    | 'B' -> B
    | 'X' -> X
    | _ -> failwith "Invalid character") l
  

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  let rec aux phase tokens =
    match tokens, phase with
    | [], _ -> true
    | A::rest,  `A -> aux `A rest
    | X::rest,  (`A | `X) -> aux `X rest
    | B::rest,  (`A | `X | `B) -> aux `B rest
    | _ -> false
  in
  aux `A  l


(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = 
  let a, b =
    List.fold_left(fun (numA, numB) x -> match x with
                  | A -> (numA+1,numB)
                  | B -> (numA,numB+1)
                  | X -> (numA,numB)) (0,0) l in
  if a = b then X
  else if a > b then A
  else B


(* val string_of_winner : token -> string *)
let string_of_winner w = 
  match w with
  | A -> "The winner is A"
  | B -> "The winner is B"
  | X -> "Tie"
