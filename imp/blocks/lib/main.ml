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

(* Small-Step: Funzioni di Trace *)
let rec trace1 (n:int) = function
  | Cmd(Skip, st) when (n>0) -> St(st)
  | Cmd(Assign(name, v), st) when (n > 0) -> St(bind_mem (getmem st) (iloc_of_envval ((topenv st) name)) (getmem st) (int_of_memval (eval_expr v)))
  | _ -> raise NoRuleApplies
;;

let rec inner_trace n conf = 
  try
    let conf' = trace1 n conf
    in conf::(inner_trace n conf')
  with 
    | NoRuleApplies -> [conf]
;;

let trace n cmd = inner_trace n (Cmd(Block(cmd), state0))