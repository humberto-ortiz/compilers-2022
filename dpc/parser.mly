
%{
open Syntax
%}
%token <int64> NUMBER
%token LPAREN
%token RPAREN
%token ADD1
%token SUB1
%token PRINT
%token FOO
%token MAX
%token DEF
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
%token COMA
%token PLUS MINUS TIMES
%token EOF

%left PLUS MINUS TIMES AND OR

%start <program> program

%%

(* reglas aqui *)
program:
  | ds = decls e = expr EOF { Program (ds, e) }
  | e = expr EOF { Program ([], e) }

decls:
 | d = decl { [d]}
 | d = decl ds = decls { d::ds }

decl:
  | DEF id = IDENTIFIER LPAREN arg = IDENTIFIER RPAREN COLON e = expr { DFun (id, arg, e )}

expr:
  | n = NUMBER { Num n }
  | TRUE { EBool true }
  | FALSE { EBool false }
  | e1 = expr AND e2 = expr { EPrim2 (And, e1, e2)}
  | e1 = expr OR e2 = expr { EPrim2 (Or, e1, e2)}
  | NOT e = expr { EPrim1 (Not, e) }
  | ADD1 LPAREN e = expr RPAREN  { EPrim1 (Add1, e) } 
  | SUB1 LPAREN e = expr RPAREN  { EPrim1 (Sub1, e) }
  | id = IDENTIFIER LPAREN e = expr RPAREN { EApp (id, e) }
  | FOO LPAREN e1 = expr COMA e2 = expr RPAREN { EPrim2 (Foo, e1, e2) } 
  | MAX LPAREN e1 = expr COMA e2 = expr RPAREN { EPrim2 (Max, e1, e2) } 
  | LET id = IDENTIFIER EQUAL e1 = expr IN e2 = expr { Let (id, e1, e2) }
  | id = IDENTIFIER { Id id }
  | IF e1 = expr COLON e2 = expr ELSE e3 = expr { If (e1, e2, e3) }
  | LPAREN e = expr RPAREN { e }
  | l = expr PLUS r = expr { EPrim2 (Plus, l, r) }
  | l = expr MINUS r = expr { EPrim2 (Minus, l, r) }
  | l = expr TIMES r = expr { EPrim2 (Times, l, r) }
  | l = expr D_EQUAL r = expr { EPrim2 (Equal, l, r) }
