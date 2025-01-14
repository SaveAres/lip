(* Apertura Moduli *)
module T = ANSITerminal
open Printf
open Ast

(* Tipo Rule, Conway e metodo di estrazione dei valori survive/born *)
type rule = Rule of int list * int list 
let conway = Rule([2;3],[3])

let extract rule i = match rule with
  | Rule(s,_) when i=1 -> s
  | Rule(_,b) when i=2 -> b
  | _ -> []
;;

(* Funzione di parsing *)
let parse (s : string) : ast =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read_token lexbuf in
  ast

(* eval: ast -> rule *)
(* conway ast: Survive(Const(2), Survive(Const(3), Born(Const(3), Nothing))) *)
(* conway rule: ([2;3],[3]) *)
let int_of_const x = match x with
  | Const(a) -> a
  | _ -> failwith "Const must contain numbers"
;;

let rec born_eval ast = match ast with
  | Survive(_,l) -> born_eval l
  | Born(a, l) -> (int_of_const a) :: born_eval l
  | _ -> []
;;

let rec survive_eval ast = match ast with
  | Survive(a, l) -> (int_of_const a) :: survive_eval l
  | _ -> []
;;

let eval ast = Rule((survive_eval ast), (born_eval ast))

(* check_rule: int list -> int -> bool *)
let rec check_rule (list : int list) (alive_nb : int) = match list with
  | a::l -> (alive_nb=a) || check_rule l alive_nb
  | [] -> false
;;

(* let rec range a b = if b<a then [] else a::(range (a+1) b) *)

let rec zeroes = function
    0 -> []
  | n -> false::(zeroes (n-1))

let string_of_world1 w1 =
  List.fold_left (fun s x -> s ^ (if x then "*" else " ")) "" w1

let string_of_world w =
  List.fold_left (fun s x -> s ^ "\n" ^ string_of_world1 x) "" w

(* p in [0,100] is the probability of 1 *)

let rec rnd_world1 p = function
    0 -> []
  | n -> (Random.int(100)<p)::(rnd_world1 p (n-1))

let rec rnd_world p m = function
  0 -> []
| n -> (rnd_world1 p m) :: (rnd_world p m (n-1))

let init_w = rnd_world 20 30 60

let neighbours w i j = 
  let n = List.length w in
  let l0 = List.nth w ((i+n-1) mod n) 
  and l1 = List.nth w (i mod n) 
  and l2 = List.nth w ((i+1) mod n) 
  in 
  let m = List.length l0
  in  
  assert (List.length l1 = m && List.length l2 = m);
  (
    List.nth l1 (j mod m),
    [
      [List.nth l0 ((j+m-1) mod m); List.nth l0 (j mod m); List.nth l0 ((j+1) mod m)]; 
      [List.nth l1 ((j+m-1) mod m); false; List.nth l1 ((j+1) mod m)]; 
      [List.nth l2 ((j+m-1) mod m); List.nth l2 (j mod m); List.nth l2 ((j+1) mod m)]
    ])

let count1 l = List.fold_left (fun s x -> s + (if x then 1 else 0)) 0 l

let count w = List.fold_left (fun s x -> s + count1 x) 0 w

(* alive: bool list list -> int -> int -> rule -> bool *)
let alive w i j rule =
  let (cell,nb) = neighbours w i j in
  let alive_nb = count nb in
  if cell then (* cell is alive. cell survives? *)
    check_rule (extract rule 1) alive_nb
  else (* cell is dead. cell is born? *)
    check_rule (extract rule 2) alive_nb


let step1 w i rule =
  let n = List.length w in
  List.mapi (fun j _ -> alive w i j rule) (zeroes n)

let step w rule =
  let n = List.length w in
  List.mapi (fun i _ -> step1 w i rule) (zeroes n)

(* let step w = List.map step1 w *)
(* let step w = w *)

let display w =
  T.erase T.Screen;
  T.set_cursor 1 1;
  (* T.print_string [] (string_of_world w); *)
  printf "%s\n%!" (string_of_world w);
  Unix.sleepf 0.15;;

let rec loop w n rule =
  if n=0 then (display w; w)
  else (display w; loop (step w rule) (n-1) rule)

(* conversione da int list -> string *)
let rec string_of_intlist list = match list with
  | a::l -> (string_of_int a) ^ " " ^ string_of_intlist l
  | [] -> ""
;;