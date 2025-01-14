%{
  open Ast
%}

%token SURVIVE
%token BORN
%token <string> CONST
%token COMMA
%token EXTENDED
%token EOF

%start <ast> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | EXTENDED; e = extended { e }
  | SURVIVE; e = survive { e }
;

survive:
  | n = CONST; e = survive { Survive(Const(int_of_string n), e) }
  | BORN; e = born { e }
;

born:
  | n = CONST; e = born { Born(Const(int_of_string n), e) }
  | { Nothing }
;

extended:
  | SURVIVE; e = survive2 { e }
  | e = survive2 { e }
;

survive2:
  | n = CONST; COMMA; e = survive2 { Survive((Const(int_of_string n)), e) }
  | n = CONST; e = born2 { Survive(Const(int_of_string n), e) }
  | e = born2 { e }
;

born2:
  | BORN; e = inner_born2 { e }
  | e = inner_born2 { e }
;

inner_born2:
  | n = CONST; COMMA; e = born2 { Born((Const(int_of_string n)), e) }
  | n = CONST; e = born2 { Born(Const(int_of_string n), e) }
  | { Nothing } 
;