
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
%token IF
%token COLON
%token ELSE
%token D_EQUAL
%token TRUE
%token FALSE
%token NOT_EQ
%token NOT
%token AND
%token OR
%token LESS_EQUAL
%token LESS
%token EOF

%left PLUS MINUS TIMES AND OR

%start <expr> start

%%

(* reglas aqui *)
start:
  | e = expr EOF { e }

expr:
  | n = NUMBER { Num n }
  | TRUE { EBool true }
  | FALSE { EBool false }
  | e1 = expr AND e2 = expr { EPrim2 (And, e1, e2)}
  | e1 = expr OR e2 = expr { EPrim2 (Or, e1, e2)}
  | NOT e = expr { EPrim1 (Not, e) }
  | ADD1 LPAREN e = expr RPAREN  { EPrim1 (Add1, e) } 
  | SUB1 LPAREN e = expr RPAREN  { EPrim1 (Sub1, e) }
  | LET id = IDENTIFIER EQUAL e1 = expr IN e2 = expr { Let (id, e1, e2) }
  | id = IDENTIFIER { Id id }
  | IF e1 = expr COLON e2 = expr ELSE e3 = expr { If (e1, e2, e3) }
