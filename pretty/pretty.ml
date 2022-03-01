open Printf
open Syntax

let pretty_prim2 op =
  match op with
  | Plus -> "Plus "
  | Minus -> "Minus "
  | Times -> "Times "

let rec pretty_expr expr =
  match expr with
  | Num n -> "Num " ^ Int64.to_string n ^ "L"
  | Add1 e -> "Add1 (" ^ pretty_expr e ^ ")"
  | Sub1 e -> "Sub1 (" ^ pretty_expr e ^ ")"
  | EPrim2 (op, e1, e2) ->
     "EPrim2 (" ^
     pretty_prim2 op ^ ", " ^
     pretty_expr e1 ^ ", " ^
     pretty_expr e2 ^
     ")"

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let lexbuf = Lexing.from_channel input_file in
  let input_program = Parser.start Lexer.read lexbuf in
  close_in input_file;
  let program = (pretty_expr input_program) in
  printf "%s\n" program;;
