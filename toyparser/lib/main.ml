(* Apertura Moduli *)
open Ast

(* Definizione Eccezioni *)
exception InvalidOperation

(* parse : string -> ast *)
let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

(* Definizione Output del Parser *)
type int_or_err = (int, string) Result.t

let ( ==> ) (res : int_or_err) (f : int -> int_or_err) : int_or_err =
  match res with
  | Ok value -> f value
  | Error msg -> Error msg

let string_of_intorerr : int_or_err -> string = function
  | Ok n -> string_of_int n
  | Error msg -> msg

(* Funzione che converte un ast in un risultato, eval : ast -> result *)
let operation_eval e1 e2 op = match e1, e2 with
    | Error err1, _ -> Error err1
    | _, Error err2 -> Error err2
    | Ok v1, Ok v2 when (op = '+') -> Ok (v1 + v2)
    | Ok v1, Ok v2 when (op = '-') -> Ok (v1 - v2)
    | Ok v1, Ok v2 when (op = '*') -> Ok (v1 * v2)
    | Ok v1, Ok v2 when (op = '/') -> if (v2 = 0) then Error ("Error: tried to divide " ^ (string_of_int v1) ^ " by zero") else Ok (v1 / v2)
    | _ -> raise InvalidOperation
;;

let rec eval : ast -> int_or_err = function
  | Const n -> Ok n
  | Add (e1,e2) -> operation_eval (eval e1) (eval e2) '+'
  | Sub (e1,e2) -> operation_eval (eval e1) (eval e2) '-'
  | Mul (e1,e2) -> operation_eval (eval e1) (eval e2) '*'
  | Div (e1,e2) -> operation_eval (eval e1) (eval e2) '/'
;;
