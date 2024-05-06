open Base

type 'a environment = 'a Environment.environment

type t =
  | Integer of int
  | Boolean of bool
  | Return of t
  | Function of func
  | String of string
  | Null
[@@deriving show { with_path = false }]

and func =
  { parameters : Ast.identifier list
  ; body : Ast.block
  ; env : t environment [@opaque]
  }

let monkey_true = Boolean true
let monkey_false = Boolean false
