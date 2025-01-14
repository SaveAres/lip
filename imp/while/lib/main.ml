(* Apertura Moduli *)
open Ast
open Types

(* Inizializzazione Eccezioni *)
exception NoRuleApplies

(* Funzione di Parsing *)
let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

(* Big-Step Evaluations *)
let rec eval_nat (s : state) = function
  | Const(x) -> Nat(x)
  | Var(x) -> s x
  | Add(a,b) -> Nat( (int_of_nat(eval_nat s a)) + (int_of_nat(eval_nat s b)) )
  | Sub(a,b) -> Nat( (int_of_nat(eval_nat s a)) - (int_of_nat(eval_nat s b)) )
  | Mul(a,b) -> Nat( (int_of_nat(eval_nat s a)) * (int_of_nat(eval_nat s b)) )
  | _ -> raise (TypeError "Wrong evaluation of int.")
;;

let rec eval_bool (s : state) = function
  | True -> true
  | False -> false
  | Not(e) -> if (eval_bool s e) then false else true
  | And(e1,e2) -> ((eval_bool s e1) && (eval_bool s e2))
  | Or(e1,e2) -> ((eval_bool s e1) || (eval_bool s e2))
  | Eq(e1,e2) -> ((eval_nat s e1) = (eval_nat s e2))
  | Leq(e1,e2) -> ((eval_nat s e1) <= (eval_nat s e2))
  | _ -> raise (TypeError "Wrong evaluation of bool.")
;;

(* Small-Step Relations *)
let rec trace1 (n:int) = function
  | Cmd(Skip, s) when (n > 0) -> St(s)
  | Cmd(Assign(name, e), s) when (n>0) -> St(bind s name (eval_nat s e))
  | Cmd(Seq(cmd1, cmd2), s) when (n>0) -> 
      let step = trace1 (n-1) (Cmd(cmd1, s)) in
      if (is_a_state step) then         (* Seq_St *)
        let st1 = (extract_state step) in
        trace1 (n-1) (Cmd(cmd2, st1))
      else                              (* Seq_Cmd *) 
        let st' = extract_state step in
        let c1' = extract_cmd step in
        trace1 (n-1) (Cmd(Seq(c1', cmd2), st'))
  | Cmd(If(e, cmd1, cmd2), s) -> if (eval_bool s e) then Cmd(cmd1, s) else Cmd(cmd2, s)
  | Cmd(While(e, cmd), s) -> if (eval_bool s e) then Cmd(Seq(cmd, While(e,cmd)), s) else St(s)
  | _ -> raise NoRuleApplies
;;

let rec inner_trace n conf = 
  try
    let conf' = trace1 n conf
    in conf::(inner_trace n conf')
  with 
    | NoRuleApplies -> [conf]
;;

let trace n cmd = inner_trace n (Cmd(cmd, bottom))