(* depython - data types for representing python code 
   Copyright 2022 Humberto Ortiz-Zuazaga <humberto.ortiz@upr.edu>
   See LICENSE file for details.
*)

type binop = Add | Sub | Mul | Div

type expr =
 | BinOp of binop * expr * expr
 | Num of int

type stm =
 | Expr of expr
 | Print of expr
 | Assign of string * expr

type prog = Module of stm list

let p1 = Module [Assign ("a", BinOp (Add, Num 5, Num 4))];;

let rec interp e =
  match e with
    | Num x -> x
    | BinOp (Add, e1, e2) -> interp e1 + interp e2
