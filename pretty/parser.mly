
%{
open Syntax
%}
%token <int64> NUMBER
%token LPAREN
%token RPAREN
%token ADD1
%token SUB1
%token PLUS
%token MINUS
%token TIMES
%token EOF

%start <expr> start

%%

(* reglas aqui *)
start: 
| e = expr EOF { e}

expr:
 | n = NUMBER { Num n }
 | ADD1 LPAREN e = expr RPAREN  { Add1 e } 
 | SUB1 LPAREN e = expr RPAREN  { Sub1 e }
 | LPAREN e = expr RPAREN { e }
 | e1 = expr PLUS e2 = expr { EPrim2 (Plus, e1, e2) }
 | e1 = expr MINUS e2 = expr { EPrim2 (Minus, e1, e2) }
 | e1 = expr TIMES e2 = expr { EPrim2 (Times, e1, e2) }
