open Printf
open Syntax
open Anf

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
  | IImul of arg * arg
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
  | IImul (a, b) -> "\timul " ^ (arg_to_string a) ^ ", " ^
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

let is_imm e =
  match e with
  | Num _ -> true
  | Id _ -> true
  | _ -> false
;;

let rec is_anf e =
  match e with
  | Add1 e -> is_imm e
  | Sub1 e -> is_imm e
  | EPrim2 (_, e1, e2) -> (is_imm e1) && (is_imm e2)
  | Let (_, e1, e2) -> (is_anf e1) && (is_anf e2)
  | If (e1, e2, e3) -> is_imm e1 && is_anf e2 && is_anf e3
  | _ -> is_imm e

let rec anf (e : expr) =
  if is_anf e then e
  else
  match e with
  | Add1 e -> 
     let varname = gensym "_add1" in
     Let (varname, anf e, Add1 (Id varname))
  | Sub1 e -> 
     let varname = gensym "_sub1" in
     Let (varname, anf e, Sub1 (Id varname))
  | EPrim2 (op, left, right) ->
     let leftname = gensym "_left" in
     let rightname = gensym "_right" in
     Let (leftname, anf left, 
          Let (rightname, anf right, 
               EPrim2 (op, Id leftname, Id rightname)))
  | Let (v, e1, e2) ->
     Let (v, anf e1, anf e2)
  | If (e1, e2, e3) ->
     let e1id = gensym "_e1" in
     Let (e1id, anf e1, 
          If (Id e1id, anf e2, anf e3))
  | _ -> failwith "no se como convertirlo a anf (todavia)"

let rec anfv2 (e : expr) : aexpr =
  match e with
  | Num n -> AImm (ImmNum n)
  | EBool b -> AImm (ImmBool b)
  | EPrim1 (op, e) -> 
     let varname = gensym "_prim1" in
     ALet (varname, anfv2 e, APrim1 (op, ImmId varname))
  | EPrim2 (op, left, right) ->
     let leftname = gensym "_left" in
     let rightname = gensym "_right" in
     ALet (leftname, anfv2 left, 
          ALet (rightname, anfv2 right, 
               APrim2 (op, ImmId leftname, ImmId rightname)))
  | Id v -> AImm (ImmId v)
  | Let (v, e1, e2) ->
     ALet (v, anfv2 e1, anfv2 e2)
  | If (e1, e2, e3) ->
     let e1id = gensym "_e1" in
     ALet (e1id, anfv2 e1, 
          AIf (ImmId e1id, anfv2 e2, anfv2 e3))


let rec compile_aexpr (e : aexpr) (env : env) : instruction list =
  let imm_to_arg (e : immexpr) : arg =
    (* e tiene que ser un imm *)
    match e with
    | ImmNum n -> Constant n
    | ImmId id -> RegOffset (RSP, lookup id env)
  in
  match e with
  | AImm imm -> [IMov (Reg RAX, imm_to_arg imm)]
  | AAdd1 imm -> [IMov (Reg RAX, imm_to_arg imm) ;
                IAdd (Reg RAX, Constant 1L)]
  | ASub1 imm -> [ IMov (Reg RAX, imm_to_arg imm) ;
                 ISub (Reg RAX, Constant 1L)]
  | APrim2 (Plus, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       IAdd (Reg RAX, imm_to_arg right) ]
  | APrim2 (Minus, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       ISub (Reg RAX, imm_to_arg right) ]
  | APrim2 (Times, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       IImul (Reg RAX, imm_to_arg right) ]
  (* ESTE COMPILADOR ESTA un poco ROTO *)
  | ALet (id, e1, e2) ->
     let (env', slot) = update id env in
     compile_aexpr e1 env
     @ [ IMov (RegOffset (RSP, slot), Reg RAX) ]
     @ compile_aexpr e2 env'
  | AIf (imm, e2, e3) ->
     let if_done = gensym "done" in
     let if_true = gensym "if_true" in
     let if_false = gensym "if_false" in
     (* comparacion *)
     [IMov (Reg RAX, imm_to_arg imm) ]
    @ [ ICmp (Reg RAX, Constant 0L) ;
        IJe if_false ]
    (*  if_true *)
    @ [ ILabel if_true ]
    @ compile_aexpr e2 env 
    @ [ IJmp if_done ;
        (* if_false *)
        ILabel if_false ]
    @ compile_aexpr e3 env
    @ [ ILabel if_done ]
;;

let compile_prog (e : aexpr) : string =
  let prog_string =  asm_to_string (compile_aexpr e []) in
  sprintf "section .text
global our_code_starts_here
our_code_starts_here:
" ^ prog_string ^ "
        ret\n" 

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  if 2 = Array.length Sys.argv then
    let input_program = Front.parse_file (Sys.argv.(1)) in
    let anfed = anfv2 input_program in
    let program = (compile_prog anfed) in
    printf "%s\n" program;;
