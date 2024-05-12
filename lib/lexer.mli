open Base

type t

val new_lex : string -> t
val print_all_tokens : t -> out:Out_channel.t -> unit
val list_to_string : Token.t list -> string
val pp : Formatter.t -> t -> unit
val next_token : t -> t * Token.t
