{
open Parser
}
let whitespace = [' ' '\t' '\n' '\r' ]
let digit = ['0'-'9']
let number = digit+
let letter = ['A'-'Z' 'a'-'z'] 

rule read =
  parse
    | whitespace { read lexbuf }
    | number as lxm { NUMBER (Int64.of_string lxm) }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | "add1" { ADD1 }
    | "sub1" { SUB1 }
    | "let" { LET }
    | '=' { EQUAL }
    | "in" { IN }
    | "if" { IF }
    | ':' { COLON }
    | "else:" { ELSE }
    | letter (letter | digit | '_' )* as lxm { IDENTIFIER lxm }
