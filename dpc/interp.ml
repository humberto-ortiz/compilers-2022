open Syntax
open Printf

let rec interp_expr (e : expr) : int64 =
  match e with
  | Num n -> n
  | Add1 ea -> Int64.add 1L (interp_expr ea)
  | Sub1 es -> Int64.add (-1L) (interp_expr es)
  | _ -> failwith "No se que hacer"

(* Some OCaml boilerplate for reading files and command-line arguments *)
let () =
  if 2 = Array.length Sys.argv then
    let input_program = Front.parse_file (Sys.argv.(1)) in
    let result = (interp_expr input_program) in
    printf "%Ld\n" result;;
