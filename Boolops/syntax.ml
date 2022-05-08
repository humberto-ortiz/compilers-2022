type prim1 =
  | Not
  | Add1
  | Sub1
  | Print

type prim2 =
  | Plus
  | Minus
  | Times
  | And
  | Or
  | Equal

type expr =
  | Num of int64
  | EBool of bool
  | EPrim1 of prim1 * expr
  | EPrim2 of prim2 * expr * expr
  | Id of string
  | Let of string * expr * expr
  | If of expr * expr * expr
