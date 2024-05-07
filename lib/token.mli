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

val equal : t -> t -> bool
val show : t -> string
val sexp_of_t : t -> Sexp.t
val t_of_sexp : Sexp.t -> t
val compare : t -> t -> int
val pp : Formatter.t -> t -> unit

val keywords : t Map.M(String).t
val ident : string -> t
