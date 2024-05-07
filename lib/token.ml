open Base

type t =
  | ILLEGAL of char
  | EOF
  | IDENT of string
  | INT of string
  | ASSIGN
  | PLUS
  | COMMA
  | SEMICOLON
  | BANG
  | MINUS
  | ASTERISK
  | SLASH
  | LT
  | GT
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | FUNCTION
  | LET
  | IF
  | ELSE
  | RETURN
  | TRUE
  | FALSE
  | EQ
  | NOT_EQ
  | STRING of string
  | COLON
[@@deriving compare, sexp, equal, show]

let empty = Map.empty (module String)

let keywords =
  Map.set empty ~key:"let" ~data:LET
  |> Map.set ~key:"fn" ~data:FUNCTION
  |> Map.set ~key:"if" ~data:IF
  |> Map.set ~key:"else" ~data:ELSE
  |> Map.set ~key:"return" ~data:RETURN
  |> Map.set ~key:"true" ~data:TRUE
  |> Map.set ~key:"false" ~data:FALSE
;;

let ident input =
  match Map.find keywords input with
  | Some v -> v
  | None -> IDENT input
;;
