%{
open Ast
%}

%token TRUE
%token FALSE
%token <string> VAR
%token <string> CONST
%token <string> NAME
%token NOT
%token AND
%token OR
%token ADD
%token SUB
%token MUL
%token EQ
%token LEQ
%token IF
%token THEN
%token ELSE
%token ASSIGN
%token SEQ
%token WHILE
%token DO
%token LPAREN
%token RPAREN
%token INTDECL
%token LBRACE
%token RBRACE
%token SKIP
%token FUN
%token EOF 

%left OR
%left AND
%left NOT
%left EQ LEQ
%left ADD SUB
%left MUL
%left SEQ
%left DO
%left ELSE

%start <prog> start

%%

start:
  | e = prog; EOF { e }
;

prog:
  | LBRACE; d = decls; c = cmd; RBRACE { Prog(d, c) }
  | d = decls; c = cmd { Prog(d, c) }
;

cmd:
  | SKIP { Skip }
  | v = VAR; ASSIGN; e = expr { Assign(v, e) }
  | c1 = cmd; SEQ; c2 = cmd { Seq(c1,c2) }
  | IF; e = expr; THEN; c1 = cmd; ELSE; c2 = cmd { If(e, c1, c2) }
  | WHILE; e = expr; DO; c = cmd { While(e, c) }
  | LPAREN; c = cmd; RPAREN { c }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | v = VAR { Var(v) }
  | f = NAME; LPAREN; var = expr; RPAREN { Call(f, var) }
  | n = CONST { Const((int_of_string n)) }
  | NOT; e = expr { Not(e) }
  | e1 = expr; AND; e2 = expr { And(e1, e2) }
  | e1 = expr; OR; e2 = expr { Or(e1, e2) }
  | e1 = expr; ADD; e2 = expr { Add(e1, e2) }
  | e1 = expr; SUB; e2 = expr { Sub(e1, e2) }
  | e1 = expr; MUL; e2 = expr { Mul(e1, e2) }
  | e1 = expr; EQ; e2 = expr { Eq(e1, e2) }
  | e1 = expr; LEQ; e2 = expr { Leq(e1, e2) }
  | LPAREN; e = expr; RPAREN { e }
;

decl:
  | INTDECL; v = VAR { IntVar(v) }
  | FUN; name = NAME; var = VAR; c = cmd; return = expr { Fun(name, var, c, return) }
;

decls:
  | d = decl; SEQ; ds = decls { d :: ds }
  | { [] }
;