
%{
open Syntax
%}
%token <int64> NUMBER
%token LPAREN
%token RPAREN
%token ADD1
%token SUB1
%token LET
%token <string> IDENTIFIER
%token EQUAL
%token IN

%start <expr> expr

%%

(* reglas aqui *)
expr:
 | n = NUMBER { Num n }
 | ADD1 LPAREN e = expr RPAREN  { Add1 e } 
 | SUB1 LPAREN e = expr RPAREN  { Sub1 e }
 | LET id = IDENTIFIER EQUAL e1 = expr IN e2 = expr { Let (id, e1, e2) }
 | id = IDENTIFIER { Id id }
