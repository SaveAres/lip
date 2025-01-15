(* Apertura Moduli *)
open Ast
open Types

(* Inizializzazione Eccezioni *)
exception NoRuleApplies

(* Funzione di Parsing *)
let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let prog = Parser.start Lexer.read lexbuf in
  prog
;;

(* Funzioni di Evaluating *)
let rec eval_decl (st:state) = function
  | IntVar(name)::list ->
      let loc = getloc st in
      let env = bind_env (topenv st) name (IVar(loc)) in
      let st' = setenv (setloc st (loc+1)) (env :: getenv st) in
      eval_decl st' list
  | [] -> st
  | _ -> raise NoRuleApplies (* WIP *)
;; 

(* Funzioni di Tracing *)
let rec trace_int (st:state) = function
  | Const(x) -> x
  | Var(name) -> (getmem st) (iloc_of_envval ((topenv st) name))
  | Add(e1, e2) -> (int_of_memval (trace_int st e1)) + (int_of_memval (trace_int st e2))
  | Sub(e1, e2) -> (int_of_memval (trace_int st e1)) - (int_of_memval (trace_int st e2))
  | Mul(e1, e2) -> (int_of_memval (trace_int st e1)) * (int_of_memval (trace_int st e2))
  | _ -> raise (TypeError "Integer conditions must contain integer values.")   
;;

let rec trace_bool (st:state) = function
  | True -> true
  | False -> false
  | Not(e) -> if (trace_bool st e) then false else true
  | And(e1, e2) -> (trace_bool st e1) && (trace_bool st e2)
  | Or(e1, e2) -> (trace_bool st e1) || (trace_bool st e2)
  | Eq(e1, e2) -> (int_of_memval (trace_int st e1)) = (int_of_memval (trace_int st e2))
  | Leq(e1, e2) -> (int_of_memval (trace_int st e1)) <= (int_of_memval (trace_int st e2))
  | _ -> raise (TypeError "Boolean conditions must contain boolean values.")
;;

let rec trace1 (n:int) = function
  | Cmd(Skip, st) when (n > 0) -> St(st)

  | Cmd(Seq(cmd1, cmd2), st) when (n > 0) ->
    let step = trace1 (n-1) (Cmd(cmd1, st)) in
    if (is_a_state step) then         (* Seq_St *)
      let st1 = (extract_state step) in
      trace1 (n-1) (Cmd(cmd2, st1))
    else                              (* Seq_Cmd *)
      let st' = extract_state step in
      let c1' = extract_cmd step in
      trace1 (n-1) (Cmd(Seq(c1', cmd2), st'))

  | Cmd(Assign(name, e), st) when (n > 0) -> 
    let memoria = getmem st in
    let loc = (iloc_of_envval ((topenv st) name)) in
    let value = trace_int st e in
    St(setmem st (bind_mem memoria loc value))

  | Cmd(If(e, cmd1, cmd2), s) -> if (trace_bool s e) then Cmd(cmd1, s) else Cmd(cmd2, s)

  | Cmd(While(e, cmd), s) -> if (trace_bool s e) then Cmd(Seq(cmd, While(e,cmd)), s) else St(s)

  | _ -> raise NoRuleApplies
;;

let rec inner_trace n conf = 
  try
    let conf' = trace1 n conf
    in conf::(inner_trace n conf')
  with 
    | NoRuleApplies -> [conf]
;;

let trace n prog = 
  let decl_list = extract_declarations prog in
  let starting_state = eval_decl state0 decl_list in
  let cmds = extract_cmds prog in
  inner_trace n (Cmd(cmds, starting_state))