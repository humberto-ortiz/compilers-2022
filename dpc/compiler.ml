open Printf

type expr = int64

type reg = 
  | RAX

type arg =
  | Constant of int64
  | Reg of reg

type instruction = 
  | IMov of arg * arg

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
                     (arg_to_string b) ^ "\n"

let rec asm_to_string (asm : instruction list) : string =
  match asm with
  | [] -> ""
  | (inst::instrs) -> (inst_to_string inst) ^ (asm_to_string instrs)


let compile_expr (e : expr) : instruction list =
  [IMov (Reg RAX, Constant e)];;

 
let compile_prog (e : expr) : string =
  let prog_string =  asm_to_string (compile_expr e) in
  sprintf "
section .text
global our_code_starts_here
our_code_starts_here:
" ^ prog_string ^ "

        ret\n" 

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  let input_file = (open_in (Sys.argv.(1))) in
  let input_program = Int64.of_string (input_line input_file) in
  close_in input_file;
  let program = (compile_prog input_program) in
  printf "%s\n" program;;
