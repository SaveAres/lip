%{
open Ast
%}

%token <string> CONST
%token <string> HEX
%token PLUS
%token SUB
%token MUL
%token DIV
%token LPAREN
%token RPAREN
%token EOF

%left PLUS SUB
%left MUL DIV

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | hex = HEX { Const(decimal_of_hex hex) }
  | e1 = expr; PLUS; e2 = expr { Add(e1,e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
  | e1 = expr; DIV; e2 = expr { Div(e1, e2) }
  | LPAREN; e=expr; RPAREN {e}
;
