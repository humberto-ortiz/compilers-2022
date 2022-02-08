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

let rec interp_expr e =
  match e with
    | Num x -> x
    | BinOp (op, e1, e2) -> (interpop op) (interp_expr e1) (interp_expr e2)

let interp_stm s =
  match s with
  | Expr e -> interp_expr e; ()
  | Print e -> print_int (interp_expr e) ; print_newline ()

let rec interp_prog p =
  match p with
  | Module [] -> ()
  | Module (stm::stms) -> interp_stm stm ; (interp_prog (Module stms))
