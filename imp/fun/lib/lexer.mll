{
open Parser
}

let white = [' ' '\t']+
let true = "true"
let false = "false"
let char = ['A'-'Z']|['a'-'z']
let var = char*
let digit = ['0'-'9']|['1'-'9']['0'-'9']*
let num = digit
let logicalNot = "not" | '!'
let logicalAnd = "&&" | "and"
let logicalOr = "||" | "or"
let add = '+'
let sub = '-'
let mul = '*'
let eq = '='
let leq = "<="
let assign = ":="
let seq = ';'
let if = "if"
let then = "then"
let else = "else"
let while = "while"
let do = "do"
let lparen = "("
let rparen = ")"
let intDecl = "int"
let lbrace = "{"
let rbrace = "}"
let skip = "skip"
let function = "fun"
let fun_name = 'f'|'g'

rule read = parse
  | white { read lexbuf }  
  | true { TRUE }
  | false { FALSE }
  | logicalNot { NOT }
  | logicalAnd { AND }
  | logicalOr { OR }
  | add { ADD }
  | sub { SUB }
  | mul { MUL }
  | if { IF }
  | then { THEN }
  | else { ELSE }
  | assign { ASSIGN }
  | seq { SEQ }
  | eq { EQ }
  | leq { LEQ }
  | while { WHILE }
  | do { DO }
  | lparen { LPAREN }
  | rparen { RPAREN }
  | intDecl { INTDECL }
  | lbrace { LBRACE }
  | rbrace { RBRACE }
  | skip { SKIP }
  | function { FUN }
  | fun_name { NAME (Lexing.lexeme lexbuf) }
  | var { VAR (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
