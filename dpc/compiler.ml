open Printf
open Syntax

let rec interp_expr (e : expr) : int64 =
  match e with
  | Num n -> n
  | Add1 ea -> Int64.add 1L (interp_expr ea)
  | Sub1 es -> Int64.add (-1L) (interp_expr es)

type reg = 
  | RAX

type arg =
  | Constant of int64
  | Reg of reg

type instruction = 
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg

let reg_to_string r =
  match r with
  | RAX -> "RAX"

let arg_to_string (arg) : string =
  match arg with
  | Constant n -> Int64.to_string n
  | Reg r -> reg_to_string r

let inst_to_string (inst : instruction) : string =
  match inst with
  | IMov (a, b) -> "mov " ^ (arg_to_string a) ^ ", " ^
                     (arg_to_string b)
  | IAdd (a, b) -> "add " ^ (arg_to_string a) ^ ", " ^
                     (arg_to_string b)
  | ISub (a, b) -> "sub " ^ (arg_to_string a) ^ ", " ^
                     (arg_to_string b)

let asm_to_string (asm : instruction list) : string =
  String.concat "\n\t" (List.map inst_to_string asm)

let rec compile_expr (e : expr) : instruction list =
  match e with
  | Num n -> [IMov (Reg RAX, Constant n)]
  | Add1 ea -> (compile_expr ea) @ [IAdd (Reg RAX, Constant 1L)]
  | Sub1 es -> (compile_expr es) @ [ISub (Reg RAX, Constant 1L)]
;;

let compile_prog (e : expr) : string =
  let prog_string =  asm_to_string (compile_expr e) in
  sprintf "section .text
global our_code_starts_here
our_code_starts_here:
\t" ^ prog_string ^ "
        ret\n" 

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let lexbuf = Lexing.from_channel input_file in
  let input_program = Parser.expr Lexer.read lexbuf in
  close_in input_file;
  let program = (compile_prog input_program) in
  printf "%s\n" program;;
