(* Apertura Moduli *)
open Ast

(* Inizializzazione Eccezioni *)
exception NoRuleApplies

(* Pretty print boolexpr -> string *)
let rec string_of_boolexpr = function
  | True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_boolexpr e0) ^ "," ^ (string_of_boolexpr e1) ^ "," ^ (string_of_boolexpr e2) ^ ")"
  | Not(e) -> "Not" ^ (string_of_boolexpr e)
  | And(e1,e2) -> (string_of_boolexpr e1) ^ " And " ^ (string_of_boolexpr e2)
  | Or(e1,e2) -> (string_of_boolexpr e1) ^ " Or " ^ (string_of_boolexpr e2)
;;

(* Funzione di Parsing *)
let parse (s : string) : boolExpr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Metodi di Tracing (Small Step Semantics) *)
let rec trace1 = function
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If((trace1 e0), e1, e2)
  | Not(e) -> If((trace1 e), False, True)
  | And(e1,e2) -> If(e1,e2,False)
  | Or(e1,e2) -> If(e1,True,e2)
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]

(* Metodi di Evaluation (Big Step Semantics) *)
let rec eval = function
    True -> true
  | False -> false
  | If(e1,e2,e3) -> if (eval e1) then (eval e2) else (eval e3)
  | Not(e) -> if (eval e) then false else true
  | And(e1,e2) -> ((eval e1) && (eval e2))
  | Or(e1,e2) -> ((eval e1) || (eval e2))
;;