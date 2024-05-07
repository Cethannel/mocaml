type 'a environment = 'a Environment.environment

type t =
  | Integer of int
  | Boolean of bool
  | Return of t
  | Function of func
  | Builtin of builtin
  | String of string
  | Array of t list
  | Null
[@@deriving show { with_path = false }]

and func =
  { parameters : Ast.identifier list
  ; body : Ast.block
  ; env : t environment [@opaque]
  }

and builtin = BuiltinFn of (t list -> (t, string) result)
[@@deriving show { with_path = false }]

let monkey_true = Boolean true
let monkey_false = Boolean false
let builtin_fn f = Builtin (BuiltinFn f)
