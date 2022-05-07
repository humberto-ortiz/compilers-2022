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
  | AApp of string * immexpr

type adecl =
    (* function name, argument name, body *)
  | AFun of string * string * aexpr

type aprogram =
  | AProgram of adecl list * aexpr
