open Syntax

type immexpr =
  | ImmNum of int64
  | ImmId of string
  | ImmBool of bool

type aexpr =
  | AImm of immexpr
  | APrim1 of prim1 * immexpr
  | APrim2 of prim2 * immexpr * immexpr
  | ALet of string * aexpr * aexpr
  | AIf of immexpr * aexpr * aexpr
