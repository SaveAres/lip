{
open Parser
}

let white = [' ' '\t']+
let logicalAnd = "&&" | "and"
let logicalOr = "||" | "or"
let logicalNot = "not" | '!'

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
  | eof { EOF }
