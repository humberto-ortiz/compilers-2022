
%{
open Syntax
%}
%token <int64> NUMBER
%token LPAREN
%token RPAREN
%token ADD1
%token SUB1
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
