%{
open Syntax
%}
%token <int64> NUMBER
%token LPAREN
%token RPAREN
%token ADD1
%token SUB1

%start <expr> expr

%%

(* reglas aqui *)
expr:
 | n = NUMBER { Num n }
