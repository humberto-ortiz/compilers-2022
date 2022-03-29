open Syntax

type immexpr =
  | ImmNum of int64
  | ImmId of string

type aexpr =
  | AImm of immexpr
  | AAdd1 of immexpr
  | ASub1 of immexpr
  | APrim2 of prim2 * immexpr * immexpr
  | ALet of string * aexpr * aexpr
  | AIf of immexpr * aexpr * aexpr
