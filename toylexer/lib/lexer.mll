{
  open Token
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let chr = ['a'-'z' 'A'-'Z' '0'-'9']
let id = letter chr*
let num = ['0'-'9']|['1'-'9']['0'-'9']*
let atok = ['A'-'Z'] chr*
let vowel = 'a'|'e'|'i'|'o'|'u'
let btok = vowel*
let consonant = [^'a''e''i''o''u''0'-'9']
let ctok = consonant* vowel consonant* | consonant*
let digits = ['0'-'9']*
let dot = '.'
let dtok = '-'? digits dot? digits
let hex1 = ['1'-'9']|['A'-'F']|['a'-'f'] 
let hex2 = hex1 ['0'-'9']|['A'-'F']|['a'-'f']*
let hex = hex1 | hex2
let x = 'x'|'X' 
let etok = '0' x hex+

rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { ASSIGN }
  | "+" { PLUS }
  | ";" { SEQ }
  | atok { ATOK (Lexing.lexeme lexbuf) }
  | btok { BTOK (Lexing.lexeme lexbuf) }
  | ctok { CTOK (Lexing.lexeme lexbuf) }
  | dtok { DTOK (Lexing.lexeme lexbuf) }
  | etok { ETOK (Lexing.lexeme lexbuf) }    
  | id { ID (Lexing.lexeme lexbuf) }
  | num { CONST (Lexing.lexeme lexbuf) }  
  | eof { EOF }
