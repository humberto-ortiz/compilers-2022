open Printf
open Syntax

type reg = 
  | RAX
  | RSP

type arg =
  | Constant of int64
  | Reg of reg
  | RegOffset of reg * int64

type instruction = 
  | IMov of arg * arg
  | IAdd of arg * arg
  | ISub of arg * arg
  | ICmp of arg * arg
  | IJe of string
  | IJmp of string
  | ILabel of string

let reg_to_string r =
  match r with
  | RAX -> "RAX"
  | RSP -> "RSP"

let arg_to_string (arg) : string =
  match arg with
  | Constant n -> Int64.to_string n
  | Reg r -> reg_to_string r
  | RegOffset (r, n) -> 
     sprintf "[%s - 8* %Ld]" (reg_to_string r) (n)

let inst_to_string (inst : instruction) : string =
  match inst with
  | IMov (a, b) -> "\tmov " ^ (arg_to_string a) ^ ", " ^
                     (arg_to_string b)
  | IAdd (a, b) -> "\tadd " ^ (arg_to_string a) ^ ", " ^
                     (arg_to_string b)
  | ISub (a, b) -> "\tsub " ^ (arg_to_string a) ^ ", " ^
                     (arg_to_string b)
  | ICmp (a, b) -> "\tcmp " ^ (arg_to_string a) ^ ", " ^
                     (arg_to_string b)
  | IJe label -> "\tje " ^ label
  | IJmp label -> "\tjmp " ^ label
  | ILabel label -> label ^ ":"

let asm_to_string (asm : instruction list) : string =
  String.concat "\n" (List.map inst_to_string asm)

type env = (string * int64) list

let rec lookup id env =
  match env with
    | [] -> failwith (sprintf "No se encontro %s" id)
    | (v, slot)::rest -> if id = v then slot else lookup id rest

let update id env = 
  let slot = Int64.add 1L (Int64.of_int (List.length env)) in
  ((id, slot)::env , slot)

let gensym =
  let counter = ref 0 in
  (fun basename ->
    counter := !counter + 1;
    sprintf "%s_%d" basename !counter);;

let rec compile_expr (e : expr) (env : env) : instruction list =
  match e with
  | Num n -> [IMov (Reg RAX, Constant n)]
  | Add1 ea -> (compile_expr ea env) @ [IAdd (Reg RAX, Constant 1L)]
  | Sub1 es -> (compile_expr es env) @ [ISub (Reg RAX, Constant 1L)]
  | Id id ->
     [ IMov (Reg RAX, RegOffset (RSP, lookup id env)) ]
  | Let (id, e1, e2) ->
     let (env', slot) = update id env in
     compile_expr e1 env
     @ [ IMov (RegOffset (RSP, slot), Reg RAX) ]
     @ compile_expr e2 env'
  | If (e1, e2, e3) ->
     let if_done = gensym "done" in
     let if_true = gensym "if_true" in
     let if_false = gensym "if_false" in
     (* comparacion *)
     compile_expr e1 env
    @ [ ICmp (Reg RAX, Constant 0L) ;
        IJe if_false ]
    (*  if_true *)
    @ [ ILabel if_true ]
    @ compile_expr e2 env 
    @ [ IJmp if_done ;
        (* if_false *)
        ILabel if_false ]
    @ compile_expr e3 env
    @ [ ILabel if_done ]
  | _ -> failwith "No se compilar eso todavia"
;;

let compile_prog (e : expr) : string =
  let prog_string =  asm_to_string (compile_expr e []) in
  sprintf "section .text
global our_code_starts_here
our_code_starts_here:
" ^ prog_string ^ "
        ret\n" 

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  if 2 = Array.length Sys.argv then
    let input_program = Front.parse_file (Sys.argv.(1)) in
    let program = (compile_prog input_program) in
    printf "%s\n" program;;
