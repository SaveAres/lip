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

(* Big-Step: Funzioni di Evaluate *)
let rec eval_expr (st:state) = function
  | True -> Bool(true)
  | False -> Bool(false)
  | Not(e) -> if (bool_of_memval (eval_expr st e)) then Bool(false) else Bool(true)
  | And(e1, e2) -> Bool((bool_of_memval (eval_expr st e1)) && (bool_of_memval (eval_expr st e2)))
  | Or(e1, e2) -> Bool((bool_of_memval (eval_expr st e1)) || (bool_of_memval (eval_expr st e2)))
  | Eq(e1, e2) -> Bool((int_of_memval (eval_expr st e1)) = (int_of_memval (eval_expr st e2)))
  | Leq(e1, e2) -> Bool((int_of_memval (eval_expr st e1)) <= (int_of_memval (eval_expr st e2)))
  | Const(x) -> Int(x)
  | Var(name) -> (getmem st) (loc_of_envval ((topenv st) name))
  | Add(e1, e2) -> Int((int_of_memval (eval_expr st e1)) + (int_of_memval (eval_expr st e2)))
  | Sub(e1, e2) -> Int((int_of_memval (eval_expr st e1)) - (int_of_memval (eval_expr st e2)))
  | Mul(e1, e2) -> Int((int_of_memval (eval_expr st e1)) * (int_of_memval (eval_expr st e2)))   
;;

let rec eval_decl (st:state) = function
  | IntVar(name)::list ->
      let loc = getloc st in
      let env = bind_env (topenv st) name (IVar(loc)) in
      let st' = setenv (setloc st (loc+1)) (env :: getenv st) in
      eval_decl st' list
  | BoolVar(name)::list ->
      let loc = getloc st in
      let env = bind_env (topenv st) name (BVar(loc)) in
      let st' = setenv (setloc st (loc+1)) (env :: getenv st) in
      eval_decl st' list
  | [] -> st
;; 

(* Small-Step: Funzioni di Trace *) 
let rec trace1 (n:int) = function
  | Cmd(Block(Decl(decl_list, prog)), st) when (n > 0) -> Cmd(prog, (eval_decl st decl_list))  (* Blocco con dichiarazioni *)
  | Cmd(Block(cmd), st) when (n > 0) -> Cmd(cmd, st)                                           (* Blocco senza dichiarazioni *)
  | Cmd(Skip, st) when (n>0) -> St(st)
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
    let loc = (loc_of_envval ((topenv st) name)) in
    let value = eval_expr st e in
    let type_check = ((is_integer_variable ((topenv st) name)) = (is_integer_value value)) in (* Controllo che il tipo sia coerente con la dichiarazione*)
    if (type_check) then St(setmem st (bind_mem memoria loc value)) else raise (TypeError "Error: inconsistent variable assignment.s")
  | Cmd(If(e, cmd1, cmd2), s) -> if (bool_of_memval (eval_expr s e)) then Cmd(cmd1, s) else Cmd(cmd2, s)
  | Cmd(While(e, cmd), s) -> if (bool_of_memval (eval_expr s e)) then Cmd(Seq(cmd, While(e,cmd)), s) else St(s)
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