open Base

let ( let* ) res f = Base.Result.bind res ~f
let ( let@ ) res f = Base.Option.bind res ~f

type prec =
  [ `Lowest
  | `Equals
  | `LessGreater
  | `Sum
  | `Product
  | `Prefix
  | `Call
  | `Index
  ]
[@@deriving show, ord]

let get_precendence = function
  | Token.EQ -> `Equals
  | Token.NOT_EQ -> `Equals
  | Token.LT -> `LessGreater
  | Token.GT -> `LessGreater
  | Token.PLUS -> `Sum
  | Token.MINUS -> `Sum
  | Token.SLASH -> `Product
  | Token.ASTERISK -> `Product
  | _ -> `Lowest
;;

type t =
  { lexer : Lexer.t
  ; cur_token : Token.t option
  ; peek_token : Token.t option
  }
[@@deriving show]

type parse_error =
  { msg : string
  ; parser : t
  ; statements : Ast.statement list
  }
[@@deriving show]

let advance parser =
  let cur_token = parser.peek_token in
  let lexer, peek_token = Lexer.next_token parser.lexer in
  if Token.equal peek_token Token.EOF
  then { lexer; cur_token; peek_token = None }
  else { lexer; cur_token; peek_token = Some peek_token }
;;

let init lexer =
  let parser = { lexer; cur_token = None; peek_token = None } in
  advance parser |> advance
;;

let parse_ident parser =
  let open Ast in
  match parser.peek_token with
  | Some (Token.IDENT identifier) -> Ok (advance parser, { identifier })
  | _ -> Error "missing ident"
;;

let peek_precedence parser =
  match parser.peek_token with
  | Some tok -> get_precendence tok
  | None -> `Lowest
;;

let cur_precedence parser =
  match parser.cur_token with
  | Some tok -> get_precendence tok
  | None -> `Lowest
;;

let expect_peek parser condition =
  match parser.peek_token with
  | Some tok ->
    if condition tok
    then Ok (advance parser)
    else Error (Fmt.failwith "missing peeked: %a" pp parser)
  | None -> Error "no peek token"
;;

let expect_assign parser =
  expect_peek parser (function
    | Token.ASSIGN -> true
    | _ -> false)
;;

let peek_is_semi parser =
  match parser.peek_token with
  | Some Token.SEMICOLON -> true
  | _ -> false
;;

let rec skip_to_semi parser =
  match parser.cur_token with
  | Some Token.SEMICOLON -> Ok parser
  | None -> Error "Ran out of tokens looking for ;"
  | _ -> skip_to_semi @@ advance parser
;;

let expr_parse_ident parser =
  match parser.cur_token with
  | Some (Token.IDENT identifier) -> Ok (Ast.Identifier { identifier })
  | _ -> Error "missing ident"
;;

let expr_parse_integer parser =
  match parser.cur_token with
  | Some (Token.INT num) ->
    let num =
      try Int.of_string num with
      | Failure x -> Fmt.failwith "COULD NOT PARSE: '%s' DUE TO %s" num x
    in
    Ok (Ast.Integer num)
  | _ -> Error "missing number"
;;

let rec parse parser =
  let rec loop parser program =
    match parser.cur_token with
    | Some _ ->
      let out = parse_statement parser in
      (match out with
       | Ok (parser, stmt) -> loop (advance parser) @@ (stmt :: program)
       | Error err -> Error { msg = err; parser; statements = program })
    | None -> Ok (parser, List.rev program)
  in
  let* _, statements = loop parser [] in
  Ok (Ast.Program { statements })

and parse_statement parser =
  match parser.cur_token with
  | Some Token.LET -> parse_let_statement parser
  | Some Token.RETURN -> parse_return_statement parser
  | Some _ -> parser_expression_statement parser
  | None -> Error "No more tokens"

and parse_expression parser precedence =
  let* parser, left = parse_prefix_expression parser in
  let rec parse_expression' parser left =
    if peek_is_semi parser || (prec_gte precedence @@ peek_precedence parser)
    then Ok (parser, left)
    else (
      match get_infix_fn parser with
      | Some fn ->
        let* parser, left = fn parser left in
        parse_expression' parser left
      | None -> Ok (parser, left))
  in
  parse_expression' parser left

and parse_prefix_expression parser =
  let map_parser = Result.map ~f:(fun v -> parser, v) in
  let token = parser.cur_token |> Option.value_exn in
  match token with
  | Token.IDENT _ -> expr_parse_ident parser |> map_parser
  | Token.INT _ -> expr_parse_integer parser |> map_parser
  | Token.BANG | Token.MINUS -> expr_parse_prefix parser token
  | tok ->
    Error (Fmt.str "unexpected prefix expr: %a\n %a" Token.pp tok pp parser)

and get_infix_fn parser =
  let open Token in
  match parser.peek_token with
  | Some PLUS
  | Some MINUS
  | Some SLASH
  | Some ASTERISK
  | Some EQ
  | Some NOT_EQ
  | Some LT
  | Some GT -> Some parse_infix_expression
  | _ -> None

and parse_infix_expression parser left =
  let* operator =
    Result.of_option parser.cur_token ~error:"No token for operator"
  in
  let precedence = cur_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser precedence in
  Ok (parser, Ast.Infix { left; operator; right })

and expr_parse_prefix parser operator =
  let parser = advance parser in
  let* parser, right = parse_expression parser `Lowest in
  Ok (parser, Ast.Prefix { operator; right })

and parse_let_statement parser =
  let open Ast in
  let* parser, name = parse_ident parser in
  let* parser = expect_assign parser in
  let* parser = skip_to_semi parser in
  Ok (parser, Let { name; value = Ast.NoneTemp })

and parse_return_statement parser =
  let open Ast in
  let* parser = skip_to_semi parser in
  Ok (parser, Return NoneTemp)

and parser_expression_statement parser =
  let* parser, expr = parse_expression parser `Lowest in
  let parser = if peek_is_semi parser then advance parser else parser in
  Ok (parser, Ast.ExpressionStatement expr)
;;

let string_of_statement = function
  | Ast.Let stmt ->
    Fmt.str
      "LET: let %s = %s"
      (Ast.show_identifier stmt.name)
      (Ast.show_expression stmt.value)
  | Return expr -> Fmt.str "RETURN %s" (Ast.show_expression expr)
  | ExpressionStatement expr -> Fmt.str "EXPR: %s;" (Ast.show_expression expr)
(*  | BlockStatement _ -> assert false*)

and string_of_ident ident = Ast.(ident.identifier)

let print_node = function
  | Ast.Program program ->
    Fmt.pr "Program: [@.";
    List.iter program.statements ~f:(fun s ->
      Fmt.pr "  %s@." (string_of_statement s));
    Fmt.pr "]@."
  | _ -> failwith "yaya"
;;

module Tests = struct
  let expect_program input =
    let lexer = Lexer.new_lex input in
    let parser = init lexer in
    let program = parse parser in
    match program with
    | Ok program -> print_node program
    | Error msg -> Fmt.failwith "%a@." pp_parse_error msg
  ;;

  let%expect_test "letStatements " =
    expect_program {|
let x = 5;
let y = 10;
let a = 312;
let b = 32143;
    |};
    [%expect
      {|
    Program: [
      LET: let { identifier = "x" } = NoneTemp
      LET: let { identifier = "y" } = NoneTemp
      LET: let { identifier = "a" } = NoneTemp
      LET: let { identifier = "b" } = NoneTemp
    ] |}]
  ;;

  let%expect_test "returnStatements " =
    expect_program
      {|
      return 5;
      return 10;
      return 993322;
    |};
    [%expect
      {|
    Program: [
      RETURN NoneTemp
      RETURN NoneTemp
      RETURN NoneTemp
    ] |}]
  ;;

  let%expect_test "identExpression " =
    expect_program {|
      foobar;
    |};
    [%expect
      {|
    Program: [
      EXPR: (Identifier { identifier = "foobar" });
    ] |}]
  ;;

  let%expect_test "integerLiteralExpression" =
    expect_program {|
      5;
    |};
    [%expect {|
    Program: [
      EXPR: (Integer 5);
    ] |}]
  ;;

  let%expect_test "parseringPrefixExpression" =
    expect_program {|
      !5;
      -15;
    |};
    [%expect
      {|
    Program: [
      EXPR: Prefix {operator = Token.BANG; right = (Integer 5)};
      EXPR: Prefix {operator = Token.MINUS; right = (Integer 15)};
    ] |}]
  ;;

  let%expect_test "parseringPrefixExpression" =
    expect_program
      {|
      5 + 5;
      5 - 5;
      5 * 5;
      5 / 5;
      5 > 5;
      5 < 5;
      5 == 5;
      5 != 5;
    |};
    [%expect
      {|
    Program: [
      EXPR: Prefix {operator = Token.BANG; right = (Integer 5)};
      EXPR: Prefix {operator = Token.MINUS; right = (Integer 15)};
    ] |}]
  ;;
end
