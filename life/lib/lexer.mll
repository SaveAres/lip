{
  open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let digits = digit*
let survive = 's'|'S'
let born = 'b'|'B'
let slash = '/'
let extended = 'E'
let comma = ','

rule read_token = parse
  | white { read_token lexbuf }
  | survive { SURVIVE }
  | born { BORN }
  | extended { EXTENDED }
  | comma { COMMA }
  | slash { read_token lexbuf }
  | digit { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }