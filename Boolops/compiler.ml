open Printf
open Syntax
open Anf

type reg = 
  | RAX
  | RSP
  | R11
  | RDI

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
  | ITest of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IJe of string
  | IJz of string
  | IJmp of string
  | ILabel of string
  | ISar of arg * arg
  | IXor of arg * arg
  | ICall of string

let reg_to_string r =
  match r with
  | RAX -> "RAX"
  | RSP -> "RSP"
  | R11 -> "R11"
  | RDI -> "RDI"

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
  | ITest (a, b) -> "\ttest " ^ (arg_to_string a) ^ ", " ^
                     (arg_to_string b)
  | IAnd (a, b) -> "\tand " ^ (arg_to_string a) ^ ", " ^
                      (arg_to_string b)
  | IOr (a, b) -> "\tor " ^ (arg_to_string a) ^ ", " ^
                      (arg_to_string b)
  | IJe label -> "\tje " ^ label
  | IJz label -> "\tjz " ^ label
  | IJmp label -> "\tjmp " ^ label
  | ILabel label -> label ^ ":"
  | ISar (a, b) -> "\tsar " ^ arg_to_string a ^ ", " ^
                     arg_to_string b
  | IXor (a, b) -> "\txor " ^ arg_to_string a ^ ", " ^
                     arg_to_string b
  | ICall label -> "\tcall " ^ label

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


let include_bool_check (a : arg) =
  (* Adds some boilerplate to verify the type of an argument. *)
  [       
    IMov (Reg RAX, a) ;
    ITest (Reg RAX, Constant 1L) ;
    IJz "error_not_Boolean" ;
  ]

let rec compile_aexpr (e : aexpr) (env : env) : instruction list =
  let imm_to_arg (e : immexpr) : arg =
    (* e tiene que ser un imm *)
    match e with
    | ImmNum n -> Constant (Int64.mul n  2L)
    | ImmId id -> RegOffset (RSP, lookup id env)
    | ImmBool true -> Constant 0x8000000000000001L
    | ImmBool false -> Constant 0x1L
  in
  match e with
  | AImm imm -> [IMov (Reg RAX, imm_to_arg imm)]
  | APrim1 (Add1, imm) -> [IMov (Reg RAX, imm_to_arg imm) ;
                IAdd (Reg RAX, Constant 2L)]
  | APrim1 (Sub1, imm) -> [ IMov (Reg RAX, imm_to_arg imm) ;
                 ISub (Reg RAX, Constant 2L)]
  | APrim1 (Not, imm) -> 
     [ IMov (Reg RAX, imm_to_arg imm) ;
       ITest (Reg RAX, Constant 1L) ;
       IJz "error_not_Boolean" ;
       IMov (Reg R11, Constant 0x8000000000000000L) ;
       IXor (Reg RAX, Reg R11) ] (* aqui todavia hay un bug (! 7) *)
  | APrim1 (Print, imm) -> 
     [ IMov (Reg RAX, imm_to_arg imm) ;
       IMov (Reg RDI, Reg RAX) ;
       ICall "print"  ]

  | APrim2 (Plus, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       IAdd (Reg RAX, imm_to_arg right) ]
  | APrim2 (Minus, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       ISub (Reg RAX, imm_to_arg right) ]
  | APrim2 (Times, left, right) ->
     [ IMov (Reg RAX, imm_to_arg left) ;
       IImul (Reg RAX, imm_to_arg right) ;
     ISar (Reg RAX, Constant 1L) ]
  (* See table at https://course.ccs.neu.edu/cs4410/lec_tagging-values_notes.html#:~:text=sign%20bit%20unchanged.-,2.5%C2%A0Defining%20logic%20operations%20over%20our%20representations,-Logical%20and%20and *)
  | APrim2 (And, left, right) ->
      include_bool_check(imm_to_arg left) @
      [
        (* save left *)
        IMov (Reg R11, Reg RAX)
      ] @
      include_bool_check(imm_to_arg right) @
      [
        (* Directly return the value of a logical AND *)
        IAnd (Reg RAX, Reg R11) ;
      ]
  | APrim2 (Or, left, right) ->
      include_bool_check(imm_to_arg left) @
      include_bool_check(imm_to_arg right) @
      [
        (* Move left to RAX *)
        IMov (Reg RAX, imm_to_arg left) ;
        (* Directly return the value of a logical OR *)
        IOr (Reg RAX, imm_to_arg right) ;
      ]
  (* See strategy at https://course.ccs.neu.edu/cs4410/lec_tagging-values_notes.html#:~:text=point%20out%20now.-,2.6,-Defining%20comparisons%20over *)
  | APrim2 (Equal, left, right) ->
      let eq_done = gensym "eq_done" in
      let eq_true = gensym "eq_true" in
      [
        (* Move left to RAX *)
        IMov (Reg RAX, imm_to_arg left) ;
        (* Do a CMP -- just subtracts and sets flag *)
        ICmp (Reg RAX, imm_to_arg right) ;
        (* Assume it's false - for now. *)
        IMov (Reg RAX, imm_to_arg (ImmBool false)) ;
        (* If zero flag is set, left - right = 0, so equal. *)
        IJz eq_true ;
        (* Jump immediately to prevent executing eq_true if ZF=0 (not-equal). *)
        IJmp eq_done ;
        (* Label to set eq_true. *)
        ILabel eq_true ;
        IMov (Reg RAX, imm_to_arg (ImmBool true)) ;
        (* We're 'a done. *)
        ILabel eq_done ;
      ]
  (* ESTE COMPILADOR ESTA ROTO -- not anymore! *)
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
    @ [ ICmp (Reg RAX, Constant 1L) ; (* esto es un bug *)
        IJe if_false ]
    (*  if_true *)
    @ [ ILabel if_true ]
    @ compile_aexpr e2 env 
    @ [ IJmp if_done ;
        (* if_false *)
        ILabel if_false ]
    @ compile_aexpr e3 env
    @ [ ILabel if_done ]
  (* | _ -> failwith "Todavia no se como" *)
;;

let compile_prog (e : aexpr) : string =
  let prog_string =  asm_to_string (compile_aexpr e []) in
  sprintf "section .text
extern print
extern error

error_not_Boolean:
        ;; aqui no hay que traquetear con RSP, no llegamos por un call
        mov RDI, 2 ; primer argumento ERR_NOT_BOOLEAN
        mov RSI, RAX ; segundo argumento valor erroneo
        call error

global our_code_starts_here
our_code_starts_here:
        push RBP          ; save (previous, caller's) RBP on stack
        mov RBP, RSP      ; make current RSP the new RBP
" ^ prog_string ^ "
        mov RSP, RBP
        pop RBP
        ret\n"

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  if 2 = Array.length Sys.argv then
    let input_program = Front.parse_file (Sys.argv.(1)) in
    let anfed = anfv2 input_program in
    let program = (compile_prog anfed) in
    printf "%s\n" program;;
