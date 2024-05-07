open Base

type node =
  | Program of program
  | Expression of expression
  | Statement of statement

and expression =
  | Identifier of identifier
  | Integer of int
  | Prefix of
      { operator : Token.t
      ; right : expression
      }
  | Infix of
      { left : expression
      ; operator : Token.t
      ; right : expression
      }
  | Boolean of bool
  | If of
      { condition : expression
      ; consequence : block
      ; alternative : block option
      }
  | FunctionLiteral of
      { parameters : identifier list
      ; body : block
      }
  | Call of
      { fn : expression
      ; args : expression list
      }
  | String of string
  | Array of expression list
  | Index of
      { left : expression
      ; index : expression
      }
[@@deriving show { with_path = false }, sexp]

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of block
[@@deriving show { with_path = false }, sexp]

and identifier = { identifier : string } [@@deriving sexp]
and block = { block : statement list }
and program = { statements : statement list }

let token_literal = function
  | Program _ -> "program"
  | Expression _ -> "expression"
  | Statement _ -> "statement"
;;
