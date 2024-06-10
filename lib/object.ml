type 'a environment = 'a Environment.environment

type t =
  | Integer of int
  | Boolean of bool
  | Return of t
  | Function of func
  | Builtin of builtin
  | String of string
  | Array of t list
  | Hash of (t * t) list
  | Null
[@@deriving show { with_path = false }]

and func =
  { parameters : Ast.identifier list
  ; body : Ast.block
  ; env : t environment [@opaque]
  }
[@@deriving show]

and builtin = BuiltinFn of (t list -> (t, string) result)
[@@deriving show { with_path = false }]

let equal a b =
  match a, b with
  | Builtin _, _
  | _, Builtin _ -> Fmt.failwith "Cannot compare builtins"
  | Integer a, Integer b -> a = b
  | _, _ -> false

let monkey_true = Boolean true
let monkey_false = Boolean false
let builtin_fn f = Builtin (BuiltinFn f)
