(* depython - data types for representing python code
   Copyright 2022 Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE file for details.
*)

type binop = Add | Sub | Mul | Div

type expr =
 | BinOp of binop * expr * expr
 | Num of int
 | Name of string

type stm =
 | Expr of expr
 | Print of expr
 | Assign of string * expr

type prog = Module of stm list

let p1 = Module [Assign ("a", BinOp (Add, Num 5, Num 4))];;

let interpop op =
  match op with
    | Add -> (+)
    | Sub -> (-)
    | Mul -> ( * )
    | Div -> (/)

let rec lookup (id, env) =
  match env with
  | [] -> raise (Failure "No hay na'")
  | ((v, e)::rest) ->
     if (v = id) then
       e
     else
       lookup (id, rest)

let rec interp_expr (e, env) =
  match e with
    | Num x -> x
    | BinOp (op, e1, e2) -> (interpop op)
                              (interp_expr (e1, env))
                              (interp_expr (e2, env))
    | Name id -> (interp_expr (lookup (id, env), env))

let update (id, e, env) =
  (id, e)::env

let interp_stm (s, env) =
  match s with
  | Expr e ->
     let
       _ = interp_expr (e, env)
     in
     env
  | Print e -> print_int (interp_expr (e, env)) ; print_newline () ; env
  | Assign (id, e) -> update (id, e, env)

let rec interp_stms (stms, env) =
  match stms with
  | [] -> env
  | (stm::stms) ->
     let env' = interp_stm (stm, env) in
     interp_stms (stms, env')

let interp_prog p =
  match p with
  | Module stms -> interp_stms (stms, [])
