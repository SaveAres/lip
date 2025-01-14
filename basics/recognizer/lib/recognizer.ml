(* Definizione riconoscitori di linguaggi *)
(* [01]+ *)
let lang1 word = match word with
  | a::_ when (a='0' || a='1') -> true
  | _ -> false
;;

(* 0?1* *)
let rec inner_lang2 word = match word with
  | b::l when (b='1') -> inner_lang2 l
  | [] -> true
  | _ -> false
;;

let lang2 word = match word with
  | a::l when (a='0') -> inner_lang2 l 
  | l -> inner_lang2 l
;;

(* 0[01]*0 *)
let rec inner_lang3 word = match word with
  | a::[] when (a='0') -> true
  | a::l when (a='0' || a='1') -> inner_lang3 l
  | _ -> false
;;

let lang3 word = match word with
  | a::l when (a='0') -> inner_lang3 l
  | _ -> false
;;

(* 0*10*10* *)
let rec inner2_lang4 word = match word with
  | a::l when (a='0') -> inner2_lang4 l
  | [] -> true
  | _ -> false
;;

let rec inner1_lang4 word = match word with
  | a::l when (a='0') -> inner1_lang4 l
  | a::l when (a='1') -> inner2_lang4 l
  | _ -> false

let rec lang4 word = match word with
  | a::l when (a='0') -> lang4 l
  | a::l when (a='1') -> inner1_lang4 l
  | _ -> false

(* (00|11)* *)
let rec lang5 word = match word with
  | a::b::l when (a=b && (a='0' || a='1')) -> lang5 l
  | [] -> true
  | _ -> false 
    
(* Utilitá per il main *)
let recognizers = [lang1;lang2;lang3;lang4;lang5]                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
