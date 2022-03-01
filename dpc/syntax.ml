type prim2 =
  | Plus
  | Minus
  | Times

type expr =
  | Num of int64
  | Add1 of expr
  | Sub1 of expr
  | EPrim2 of prim2 * expr * expr
  | Id of string
  | Let of string * expr * expr
