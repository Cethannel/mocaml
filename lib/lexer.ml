open Base

type t =
  { input : string
  ; position : int
  ; readPosition : int
  ; ch : char option
  }
[@@deriving show]

let read_char lex =
  let ch = List.nth (String.to_list lex.input) lex.readPosition in
  { lex with
    ch
  ; position = lex.readPosition
  ; readPosition = lex.readPosition + 1
  }
;;

let new_lex input =
  let l = { input; position = 0; readPosition = 0; ch = None } in
  read_char l
;;

let is_letter ch = Char.(is_alpha ch || ch = '_')

let read_while lex ~fn =
  if lex.position >= String.length lex.input
  then lex, ""
  else (
    let rec loop lex = if fn lex.ch then loop @@ read_char lex else lex in
    let start_pos = lex.position in
    let lex = loop lex in
    let end_pos =
      if lex.position <= String.length lex.input
      then lex.position
      else String.length lex.input
    in
    let out = String.sub lex.input ~pos:start_pos ~len:(end_pos - start_pos) in
    lex, out)
;;

let read_ident lex =
  read_while lex ~fn:(fun ch ->
    match ch with
    | Some ch -> is_letter ch
    | None -> false)
;;

let is_whitespace ch = Char.is_whitespace ch

let skip_whitespace lex =
  let lex, _ =
    read_while lex ~fn:(fun ch ->
      match ch with
      | Some ch -> is_whitespace ch
      | None -> false)
  in
  lex
;;

let is_number ch = Char.is_digit ch

let read_int lex =
  read_while lex ~fn:(fun ch ->
    match ch with
    | Some ch -> is_number ch
    | None -> false)
;;

let read_string lex =
  let lex = read_char lex in
  read_while lex ~fn:(function
    | Some ch -> not @@ Char.equal ch '"'
    | None -> false)
;;

let peek_char lex = List.nth (String.to_list lex.input) lex.readPosition

let next_token lex =
  let lex = skip_whitespace lex in
  let lex, tok =
    match lex.ch with
    | Some '=' ->
      (match peek_char lex with
       | Some '=' -> read_char lex, Token.EQ
       | _ -> lex, Token.ASSIGN)
    | Some ';' -> lex, Token.SEMICOLON
    | Some '!' ->
      (match peek_char lex with
       | Some '=' -> read_char lex, Token.NOT_EQ
       | _ -> lex, Token.BANG)
    | Some '"' ->
      let lex, str = read_string lex in
      lex, Token.STRING str
    | Some '-' -> lex, Token.MINUS
    | Some '/' -> lex, Token.SLASH
    | Some '*' -> lex, Token.ASTERISK
    | Some '<' -> lex, Token.LT
    | Some '>' -> lex, Token.GT
    | Some '(' -> lex, Token.LPAREN
    | Some ')' -> lex, Token.RPAREN
    | Some ',' -> lex, Token.COMMA
    | Some '+' -> lex, Token.PLUS
    | Some ':' -> lex, Token.COLON
    | Some '{' -> lex, Token.LBRACE
    | Some '}' -> lex, Token.RBRACE
    | Some '[' -> lex, Token.LBRACKET
    | Some ']' -> lex, Token.RBRACKET
    | Some ch when is_letter ch ->
      let lex, str = read_ident lex in
      lex, Token.ident str
    | Some ch when is_number ch ->
      let lex, str = read_int lex in
      lex, Token.INT str
    | Some ch -> lex, Token.ILLEGAL ch
    | None -> lex, Token.EOF
  in
  match tok with
  | Token.IDENT _ | Token.INT _ -> lex, tok
  | _
    when Map.data Token.keywords
         |> List.find ~f:(fun t -> Token.equal t tok)
         |> Option.is_some -> lex, tok
  | _ -> read_char lex, tok
;;

let collect_all_tokens lex =
  let rec loop lex acc =
    match next_token lex with
    | _, Token.EOF -> Token.EOF :: acc
    | lex, tok -> loop lex @@ (tok :: acc)
  in
  loop lex [] |> List.rev
;;

let list_to_string input =
  List.fold input ~init:"" ~f:(fun acc elem -> acc ^ "\n" ^ Token.show elem)
;;

let rec print_all_tokens lex ~out =
  let lex, tok = next_token lex in
  Token.show tok |> Out_channel.output_string out;
  Out_channel.output_string out "\n";
  match tok with
  | Token.EOF -> ()
  | _ -> print_all_tokens lex ~out
;;

let%test_unit "next token" =
  let input =
    {|let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
      return true;
    } else {
      return false;
    }

    10 == 10;
    10 != 9;
    "foobar"
    "foo bar"
    [1, 2];
    {"foo": "bar"}
    |}
  in
  let expected =
    [ Token.LET
    ; IDENT "five"
    ; ASSIGN
    ; INT "5"
    ; SEMICOLON
    ; LET
    ; IDENT "ten"
    ; ASSIGN
    ; INT "10"
    ; SEMICOLON
    ; LET
    ; IDENT "add"
    ; ASSIGN
    ; FUNCTION
    ; LPAREN
    ; IDENT "x"
    ; COMMA
    ; IDENT "y"
    ; RPAREN
    ; LBRACE
    ; IDENT "x"
    ; PLUS
    ; IDENT "y"
    ; SEMICOLON
    ; RBRACE
    ; SEMICOLON
    ; LET
    ; IDENT "result"
    ; ASSIGN
    ; IDENT "add"
    ; LPAREN
    ; IDENT "five"
    ; COMMA
    ; IDENT "ten"
    ; RPAREN
    ; SEMICOLON
    ; BANG
    ; MINUS
    ; SLASH
    ; ASTERISK
    ; INT "5"
    ; SEMICOLON
    ; INT "5"
    ; LT
    ; INT "10"
    ; GT
    ; INT "5"
    ; SEMICOLON
    ; IF
    ; LPAREN
    ; INT "5"
    ; LT
    ; INT "10"
    ; RPAREN
    ; LBRACE
    ; RETURN
    ; TRUE
    ; SEMICOLON
    ; RBRACE
    ; ELSE
    ; LBRACE
    ; RETURN
    ; FALSE
    ; SEMICOLON
    ; RBRACE
    ; INT "10"
    ; EQ
    ; INT "10"
    ; SEMICOLON
    ; INT "10"
    ; NOT_EQ
    ; INT "9"
    ; SEMICOLON
    ; STRING "foobar"
    ; STRING "foo bar"
    ; LBRACKET
    ; INT "1"
    ; COMMA
    ; INT "2"
    ; RBRACKET
    ; SEMICOLON
    ; LBRACE
    ; STRING "foo"
    ; COLON
    ; STRING "bar"
    ; RBRACE
    ; EOF
    ]
  in
  let lexer = new_lex input in
  let out = collect_all_tokens lexer in
  [%test_eq: Token.t list] expected out
;;
