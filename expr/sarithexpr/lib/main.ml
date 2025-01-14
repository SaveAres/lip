(* Apertura Moduli *)
open Ast

(* Inizializzazione Eccezioni *)
exception NoRuleApplies
exception InvalidOperation of string
exception TypeError of string

(* Pretty print expr -> string *)
let rec string_of_expr = function
  | True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not" ^ (string_of_expr e)
  | And(e1,e2) -> (string_of_expr e1) ^ " And " ^ (string_of_expr e2)
  | Or(e1,e2) -> (string_of_expr e1) ^ " Or " ^ (string_of_expr e2)
  | Zero -> "0"
  | Succ(e) -> "Succ " ^ (string_of_expr e)
  | Pred(e) -> "Pred " ^ (string_of_expr e)
  | IsZero(e) -> "IsZero(" ^ (string_of_expr e) ^ ")"
;;

(* Funzione di Parsing *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Metodi di Tracing (Small Step Semantics) *)
let rec trace1 : expr -> expr = function
  | If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> If((trace1 e0), e1, e2)
  | Not(e) -> If((trace1 e), False, True)
  | And(e1,e2) -> If(e1,e2,False)
  | Or(e1,e2) -> If(e1,True,e2)
  | Pred(Succ(e)) -> e
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> IsZero(trace1 e)
  | Pred(Zero) -> raise (InvalidOperation "Natural number must be equal or greater then zero.")
  | _ -> raise NoRuleApplies
;;

let rec trace e = 
  try
    let e' = trace1 e
    in e::(trace e')
  with 
    | NoRuleApplies -> [e]
    | InvalidOperation _ -> [e] 
;;

(* Metodi di Evaluation (Big Step Semantics): expr -> exprval *)
let check_negative = function
  | e when (e>=0) -> e
  | _ -> raise (InvalidOperation "Natural number must be equal or greater then zero.")
;;

let int_of_exprval = function
  | Nat(n) -> n
  | _ -> raise (InvalidOperation "Natural numbers mustn't use boolean operations.")
;;

let bool_of_exprval = function
  | Bool(e) -> e
  | _ -> raise (InvalidOperation "If statement must compare a boolean condition.")
;;

let exprval_of_bool = function
  | true -> Bool(true)
  | false -> Bool(false)
;;

let rec eval : expr -> exprval = function
  | True -> Bool(true)
  | False -> Bool(false)
  | If(e1,e2,e3) -> if (bool_of_exprval(eval e1)) then (eval e2) else (eval e3)
  | Not(e) -> if (bool_of_exprval(eval e)) then Bool(false) else Bool(true)
  | And(e1,e2) -> exprval_of_bool((bool_of_exprval(eval e1)) && (bool_of_exprval(eval e2)))
  | Or(e1,e2) -> exprval_of_bool((bool_of_exprval(eval e1)) || (bool_of_exprval(eval e2)))
  | Zero -> Nat(0)
  | Succ(e) -> Nat((int_of_exprval(eval e)+1))
  | Pred(e) -> Nat((check_negative(int_of_exprval(eval e)-1)))
  | IsZero(e) -> if ((int_of_exprval (eval e)) = 0) then Bool(true) else Bool(false)
;;

(* Funzione di Typechecking *)
let expected_int_exception e = raise (TypeError ((string_of_expr e) ^ " has type Bool, but type Nat was expected."))
let expected_bool_exception e = raise (TypeError ((string_of_expr e) ^ " has type Nat, but type Bool was expected."))
let diffrent_type_exception e1 e2 = raise (TypeError((string_of_expr e1) ^ " and " ^ (string_of_expr e2) ^ " must have the same type."))

let rec typecheck : expr -> exprtype = function
  | True -> BoolT
  | False -> BoolT
  | Zero -> NatT
  | Succ(e) -> if ((typecheck e)=NatT) then NatT else (expected_int_exception e)
  | Pred(e) -> if ((typecheck e)=NatT) then NatT else (expected_int_exception e)
  | IsZero(e) -> if ((typecheck e)=NatT) then BoolT else (expected_int_exception e)
  | Not(e) -> if ((typecheck e)=BoolT) then BoolT else (expected_bool_exception e)
  | And(e1,e2) -> if ((typecheck e1)=BoolT) then (if ((typecheck e2)=BoolT) then BoolT else (expected_bool_exception e2)) else (expected_bool_exception e1)
  | Or(e1,e2) -> if ((typecheck e1)=BoolT) then (if ((typecheck e2)=BoolT) then BoolT else (expected_bool_exception e2)) else (expected_bool_exception e1)
  | If(e1,e2,e3) -> if ((typecheck e1)=BoolT) then (if ((typecheck e2)=(typecheck e3)) then (typecheck e2) else (diffrent_type_exception e2 e3)) else (expected_bool_exception e1)
;;