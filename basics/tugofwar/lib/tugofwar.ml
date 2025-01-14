(* Definizione Tokens *)
type token = A | B | X

(* Definizione Metodo Conversione str -> char list *)
let explode s = List.init (String.length s) (String.get s)

(* Definizione metodo che, passato un char, restituisce il suo token *)
let find_token c = match c with
  | x when x = 'A' -> A
  | x when x = 'B' -> B
  | x when x = '=' -> X
  | _ -> failwith "Errore: input non valido."
;;

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
let rec inner_toklist_of_string s = match s with
  | c::list -> (find_token c)::(inner_toklist_of_string list)
  | [] -> []
;;

let toklist_of_string s = inner_toklist_of_string (explode s);;

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)    
let rec inner_valid3 l = match l with
  | b::list when (b = B) -> inner_valid3 list
  | [] -> true
  | _ -> false
;;

let rec inner_valid2 l = match l with
  | x::list when (x = X) -> inner_valid2 list
  | b::list when (b = B) -> inner_valid3 list
  | [] -> true
  | _ -> false
;;

let rec inner_valid1 l = match l with
  | a::list when (a = A) -> inner_valid1 list
  | x::list when (x = X) -> inner_valid2 list
  | [] -> true
  | _ -> false
;;

let valid l = inner_valid1 l;;

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)
let result countA countB = 
  if (countA > countB) then A 
  else if (countA = countB) then X
  else B
;;

let rec inner_win l countA countB = match l with
  | a::list when (a = A) -> inner_win list (countA+1) countB
  | x::list when (x = X) -> inner_win list countA countB
  | b::list when (b = B) -> inner_win list countA (countB+1)
  | _ -> result countA countB
;;

let win l = inner_win l 0 0;;

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
  | A -> "Il vincitore é A."
  | B -> "Il vincitore é B."
  | X -> "Pareggio."
;;
