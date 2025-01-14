(* Apertura Moduli *)
open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)
let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)
let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *) 
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* how_many: 'a -> 'a list -> int *)
let rec how_many t tlist = match tlist with
  | a::l when (a=t) -> 1+(how_many t l)
  | _::l -> how_many t l
  | [] -> 0
;;

(* frequency : int -> 'a list -> ('a * int) list *)
let rec frequency n tlist = match tlist with
  | token::l when (n>0) -> (token, (how_many token tlist)) :: frequency (n-1) l
  | _ -> []
;;

