{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']|['1'-'9']['0'-'9']
let num = '-'? digit*
let x = '0''x' | '0''X'
let hexnumbers = digit | ['A'-'F'] | ['a'-'f']
let hex = x hexnumbers*

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | num { CONST (Lexing.lexeme lexbuf) }
  | hex { HEX (Lexing.lexeme lexbuf) }
  | eof { EOF }
