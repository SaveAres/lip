{
open Parser
}

let white = [' ' '\t']+
let logicalAnd = "&&" | "and"
let logicalOr = "||" | "or"
let logicalNot = "not" | '!'
let zero = "zero" | '0'
let succ = "succ"
let pred = "pred"
let iszero = "iszero"

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | logicalAnd { AND }
  | logicalOr { OR }
  | logicalNot { NOT }
  | zero { ZERO }
  | succ { SUCC }
  | pred { PRED }
  | iszero { ISZERO }
  | eof { EOF }
  