{
open Parser
}
let whitespace = [' ' '\t' '\n' '\r' ]
let digit = ['0'-'9']
let number = digit+

rule read =
  parse
    | whitespace { read lexbuf }
    | number as lxm { NUMBER (Int64.of_string lxm) }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | "add1" { ADD1 }
    | "sub1" { SUB1 }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | eof { EOF }
