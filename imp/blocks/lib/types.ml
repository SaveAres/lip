(* Apertura Moduli *)
open Ast

(* Definizione Tipi dello Stato *)
type loc = int

type envval = BVar of loc | IVar of loc
type memval = Bool of bool | Int of int

type env = ide -> envval
type mem = loc -> memval

(* Inizializzazione Eccezioni *)
exception TypeError of string
exception UnboundVar of ide
exception UnboundLoc of loc
exception NoRuleApplies

(* Definizione Metodi di elaborazione della tripla stato : (envstack, memory, firstloc) *)
(* The third component of the state is the first free location. We assume that the store is unbounded *)
type state = { envstack : env list; memory : mem; firstloc : loc }

let getenv st = st.envstack
let getmem st = st.memory
let getloc st = st.firstloc

let setenv st envstack =
  { envstack; memory = st.memory; firstloc = st.firstloc }

let setmem st memory =
  { envstack = st.envstack; memory; firstloc = st.firstloc }

let setloc st firstloc =
  { envstack = st.envstack; memory = st.memory; firstloc }

let topenv st =
  match st.envstack with
  | [] -> failwith "empty environment stack"
  | e :: _ -> e

let popenv st =
  match st.envstack with
  | [] -> failwith "empty environment stack"
  | _ :: el' -> el'

let make_state envstack memory firstloc = { envstack; memory; firstloc }

let bind_env f x v y = if String.equal x y then v else f y
let bind_mem f x v y = if Int.equal x y then v else f y

let bottom_env : env = fun x -> raise (UnboundVar x)
let bottom_mem : mem = fun l -> raise (UnboundLoc l)

let state0 = make_state [bottom_env] bottom_mem 0

(* Definizione Conf *)
type conf = St of state | Cmd of cmd * state

(* Funzioni di Controllo Tipo *)
let is_a_state : conf -> bool = function
  | St(_) -> true
  | Cmd(_,_) -> false
;;

let is_integer_variable = function
  | IVar(_) -> true
  | BVar(_) -> false
;;

let is_ivar : envval -> bool = function
  | IVar(_) -> true
  | BVar(_) -> false
;;

let is_integer_value : memval -> bool = function
  | Int(_) -> true
  | Bool(_) -> false
;;

(* Funzioni di conversione *)
let loc_of_envval : envval -> loc = function
  | IVar(loc) -> loc
  | BVar(loc) -> loc
;;

let int_of_memval : memval -> int = function
  | Int(x) -> x
  | Bool(_) -> raise (TypeError "Integer operations must contain integer value.")
;;

let bool_of_memval : memval -> bool = function
  | Bool(x) -> x
  | Int(_) -> raise (TypeError "Boolean operations must contain boolean value.")
;;

let extract_state : conf -> state = function
  | Cmd(_, s) -> s
  | St(s) -> s
;;

let extract_cmd : conf -> cmd = function
  | Cmd(c, _) -> c
  | St(_) -> raise (TypeError "Impossibile recuperare il prossimo comando.")
;;

