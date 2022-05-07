
%{
open Syntax
%}
%token <int64> NUMBER
%token LPAREN
%token RPAREN
%token ADD1
%token SUB1
%token EOF
%token <string> IDENTIFIER
%token IF
%token COLON
%token ELSE
%token LET
%token EQUAL
%token IN
%token PLUS MINUS TIMES

%start <expr> start

%%

(* reglas aqui *)
start: 
| e = expr EOF { e}

expr:
  | n = NUMBER { Num n }
  | ADD1 LPAREN e = expr RPAREN  { Add1 e } 
  | SUB1 LPAREN e = expr RPAREN  { Sub1 e }
  | LET id = IDENTIFIER EQUAL e1 = expr IN e2 = expr { Let (id, e1, e2) }
  | id = IDENTIFIER { Id id }
  | IF e1 = expr COLON e2 = expr ELSE e3 = expr { If (e1, e2, e3) }
  | LPAREN e = expr RPAREN { e }
  | e1 = expr op = prim2 e2 = expr { EPrim2 (op, e1, e2) }

prim2:
  | PLUS { Plus }
  | MINUS { Minus }
  | TIMES { Times }
