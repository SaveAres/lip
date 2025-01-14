(* Apertura Moduli *)
open Ast

(* Inizializzazione Eccezioni *)
exception TypeError of string
exception UnboundVar of string
exception NoRuleApplies

(* Definizione Tipi *)
type exprval = Bool of bool | Nat of int
type state = ide -> exprval
type conf = St of state | Cmd of cmd * state

(* Funzioni di Stato *)
let bind (st:state) (x:ide) (v:exprval) : state = fun y -> if x = y then v else st y ;;
let bottom : state = fun _ -> raise (UnboundVar "Segmentation fault.");;

(* Funzioni di conversione tra tipi *)
let int_of_nat = function
  | Nat(x) -> x
  | _ -> raise (TypeError "Impossible evaluate the number value.")
;;

let extract_state : conf -> state = function
  | Cmd(_, s) -> s
  | St(s) -> s
;;

let extract_cmd : conf -> cmd = function
  | Cmd(c, _) -> c
  | St(_) -> raise (TypeError "Impossibile recuperare il prossimo comando.")
;;

let is_a_state : conf -> bool = function
  | St(_) -> true
  | Cmd(_,_) -> false
;;
