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
  | Token.LPAREN -> `Call
  | Token.LBRACKET -> `Index
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

let peek_is parser token =
  Option.equal Token.equal parser.peek_token (Some token)
;;

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

let chomp_semi parser =
  match parser.peek_token with
  | Some Token.SEMICOLON -> advance parser
  | _ -> parser
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

let expect_lparen parser =
  expect_peek parser (function
    | Token.LPAREN -> true
    | _ -> false)
;;

let expect_rparen parser =
  expect_peek parser (function
    | Token.RPAREN -> true
    | _ -> false)
;;

let expect_lbrace parser =
  expect_peek parser (function
    | Token.LBRACE -> true
    | _ -> false)
;;

let expect_rbrace parser =
  expect_peek parser (function
    | Token.RBRACE -> true
    | _ -> false)
;;

let expect_rbracket parser =
  expect_peek parser (function
    | Token.RBRACKET -> true
    | _ -> false)
;;

let expect_colon parser =
  expect_peek parser (function
    | Token.COLON -> true
    | _ -> false)
;;

let peek_is_semi parser =
  match parser.peek_token with
  | Some Token.SEMICOLON -> true
  | _ -> false
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
  | Some Token.RETURN -> parse_return parser
  | Some _ -> parse_expression_statement parser
  | None -> Error "No more tokens"

and parse_expression parser precedence =
  let* parser, left = parse_prefix_expression parser in
  let rec parse_expression' parser left =
    let peeked =
      parser.peek_token |> Option.value ~default:(Token.ILLEGAL '0')
    in
    let prec_peeked = get_precendence peeked in
    if peek_is parser Token.SEMICOLON
       || compare_prec precedence prec_peeked >= 0
    then Ok (parser, left)
    else (
      match get_infix_fn parser with
      | Some fn ->
        let parser = advance parser in
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
  | Token.STRING _ -> expr_parse_string parser |> map_parser
  | Token.IF -> expr_parse_if parser
  | Token.FUNCTION -> expr_parse_function parser
  | Token.TRUE | Token.FALSE -> expr_parse_bool parser token
  | Token.LPAREN -> expr_parse_grouped parser
  | Token.LBRACKET -> expr_parse_array parser
  | Token.LBRACE -> expr_parse_hash_literal parser
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
  | Some LPAREN -> Some parse_call_expression
  | Some LBRACKET -> Some parse_index_expression
  | _ -> None

and parse_infix_expression parser left =
  let operator = parser.cur_token |> Option.value_exn in
  let precedence = cur_precedence parser in
  let parser = advance parser in
  let* parser, right = parse_expression parser precedence in
  Ok (parser, Ast.Infix { left; operator; right })

and parse_call_expression parser fn =
  parse_list_of_exprs parser ~close:Token.RPAREN ~final:(fun args ->
    Ast.Call { fn; args })

and parse_index_expression parser left =
  let parser = advance parser in
  let* parser, index = parse_expression parser `Lowest in
  let* parser = expect_rbracket parser in
  Ok (parser, Ast.Index { left; index })

and parse_list_of_exprs parser ~close ~final =
  let rec parse_list_of_exprs' parser exprs =
    match parser.peek_token with
    | Some tok when phys_equal close tok ->
      Ok (advance parser, final @@ List.rev exprs)
    | Some Token.COMMA ->
      let parser = advance parser in
      let parser = advance parser in
      let* parser, expr = parse_expression parser `Lowest in
      parse_list_of_exprs' parser (expr :: exprs)
    | Some tok ->
      Error (Fmt.str "unexpected next expression token %a" Token.pp tok)
    | None -> Error "enexpected end of stream"
  in
  match parser.peek_token with
  | Some tok when phys_equal tok close -> parse_list_of_exprs' parser []
  | Some _ ->
    let parser = advance parser in
    let* parser, expr = parse_expression parser `Lowest in
    parse_list_of_exprs' parser [ expr ]
  | None -> Error "hit eof"

and expr_parse_prefix parser operator =
  let parser = advance parser in
  let* parser, right = parse_expression parser `Lowest in
  Ok (parser, Ast.Prefix { operator; right })

and expr_parse_grouped parser =
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let* parser = expect_rparen parser in
  Ok (parser, expr)

and expr_parse_array parser =
  parse_list_of_exprs parser ~close:Token.RBRACKET ~final:(fun exprs ->
    Ast.Array exprs)

and expr_parse_hash_literal parser =
  let rec parse' parser exprs =
    let empty = List.length exprs = 0 in
    match parser.peek_token with
    | Some Token.RBRACE -> Ok (advance parser, Ast.Hash (List.rev exprs))
    | _ when empty -> parse_key_value parser exprs
    | Some Token.COMMA when not empty -> parse_key_value (advance parser) exprs
    | _ -> Error "unexpected next token"
  and parse_key_value parser exprs =
    let parser = advance parser in
    let* parser, key = parse_expression parser `Lowest in
    let* parser = expect_colon parser in
    let parser = advance parser in
    let* parser, value = parse_expression parser `Lowest in
    parse' parser ((key, value) :: exprs)
  in
  parse' parser []

and expr_parse_bool parser bool =
  let* bool =
    match bool with
    | Token.TRUE -> Ok true
    | Token.FALSE -> Ok false
    | _ -> Error "not a valid boolean"
  in
  Ok (parser, Ast.Boolean bool)

and expr_parse_ident parser =
  match parser.cur_token with
  | Some (Token.IDENT identifier) -> Ok (Ast.Identifier { identifier })
  | _ -> Error "missing ident"

and expr_parse_string parser =
  match parser.cur_token with
  | Some (Token.STRING str) -> Ok (Ast.String str)
  | _ -> Error "missing number"

and expr_parse_integer parser =
  match parser.cur_token with
  | Some (Token.INT num) ->
    let num =
      try Int.of_string num with
      | Failure x -> Fmt.failwith "COULD NOT PARSE: '%s' DUE TO %s" num x
    in
    Ok (Ast.Integer num)
  | _ -> Error "missing number"

and parse_let_statement parser =
  let open Ast in
  let* parser, name = parse_ident parser in
  let* parser = expect_assign parser in
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semi parser in
  Ok (parser, Let { name; value = expr })

and expr_parse_if parser =
  let* parser = expect_lparen parser in
  let parser = advance parser in
  let* parser, condition = parse_expression parser `Lowest in
  let* parser = expect_rparen parser in
  let* parser = expect_lbrace parser in
  let* parser, consequence = parse_block parser in
  let* parser, alternative =
    match parser.peek_token with
    | Some Token.ELSE ->
      let parser = advance parser in
      let* parser = expect_lbrace parser in
      let* parser, alternative = parse_block parser in
      Ok (parser, Some alternative)
    | _ -> Ok (parser, None)
  in
  Ok (parser, Ast.If { condition; consequence; alternative })

and read_identifier parser =
  match parser.cur_token with
  | Some (Token.IDENT identifier) -> Ok Ast.{ identifier }
  | _ -> Error "expected to read identifier"

and expr_parse_function parser =
  let* parser = expect_lparen parser in
  let* parser, parameters =
    match parser.peek_token with
    | Some Token.RPAREN -> parse_list_of_parameters parser []
    | Some (Token.IDENT _) ->
      let parser = advance parser in
      let* ident = read_identifier parser in
      parse_list_of_parameters parser [ ident ]
    | _ -> Error "unexpectred start of parameter list"
  in
  let* parser = expect_lbrace parser in
  let* parser, body = parse_block parser in
  Ok (parser, Ast.FunctionLiteral { parameters; body })

and parse_list_of_parameters parser parameters =
  match parser.peek_token with
  | Some Token.RPAREN -> Ok (advance parser, List.rev parameters)
  | Some Token.COMMA ->
    let parser = advance parser in
    let parser = advance parser in
    let* ident = read_identifier parser in
    parse_list_of_parameters parser @@ (ident :: parameters)
  | Some tok ->
    Error (Fmt.str "unexpected next parameter token %a" Token.pp tok)
  | None -> Error "unexpected end of stream"

and parse_block parser =
  let parser = advance parser in
  let rec parse_block' parser statements =
    match parser.cur_token with
    | Some Token.RBRACE -> Ok (parser, List.rev statements)
    | Some _ ->
      let* parser, statement = parse_statement parser in
      parse_block' (advance parser) @@ (statement :: statements)
    | None -> Error "unexpected EOF"
  in
  let* parser, block = parse_block' parser [] in
  Ok (parser, Ast.{ block })

and parse_return parser =
  let open Ast in
  let parser = advance parser in
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semi parser in
  Ok (parser, Return expr)

and parse_expression_statement parser =
  let* parser, expr = parse_expression parser `Lowest in
  let parser = chomp_semi parser in
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
  | BlockStatement _ -> assert false

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
      LET: let { identifier = "x" } = (Integer 5)
      LET: let { identifier = "y" } = (Integer 10)
      LET: let { identifier = "a" } = (Integer 312)
      LET: let { identifier = "b" } = (Integer 32143)
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
      RETURN (Integer 5)
      RETURN (Integer 10)
      RETURN (Integer 993322)
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
      true;
      false;
    |};
    [%expect
      {|
    Program: [
      EXPR: Infix {left = (Integer 5); operator = Token.PLUS; right = (Integer 5)};
      EXPR: Infix {left = (Integer 5); operator = Token.MINUS; right = (Integer 5)};
      EXPR: Infix {left = (Integer 5); operator = Token.ASTERISK; right = (Integer 5)};
      EXPR: Infix {left = (Integer 5); operator = Token.SLASH; right = (Integer 5)};
      EXPR: Infix {left = (Integer 5); operator = Token.GT; right = (Integer 5)};
      EXPR: Infix {left = (Integer 5); operator = Token.LT; right = (Integer 5)};
      EXPR: Infix {left = (Integer 5); operator = Token.EQ; right = (Integer 5)};
      EXPR: Infix {left = (Integer 5); operator = Token.NOT_EQ; right = (Integer 5)};
      EXPR: (Boolean true);
      EXPR: (Boolean false);
    ] |}]
  ;;

  let%expect_test "grouped expressions" =
    expect_program "((1 + foo) *   12)";
    [%expect
      {|
    Program: [
      EXPR: Infix {
      left =
      Infix {left = (Integer 1); operator = Token.PLUS;
        right = (Identifier { identifier = "foo" })};
      operator = Token.ASTERISK; right = (Integer 12)};
    ] |}]
  ;;

  let%expect_test "if expressions" =
    expect_program "if (x < y) { x }";
    expect_program "if (x < y) { x } else { y }";
    expect_program "if (x < y) { return x } else { return y }";
    [%expect
      {|
    Program: [
      EXPR: If {
      condition =
      Infix {left = (Identifier { identifier = "x" }); operator = Token.LT;
        right = (Identifier { identifier = "y" })};
      consequence =
      { block = [(ExpressionStatement (Identifier { identifier = "x" }))] };
      alternative = None};
    ]
    Program: [
      EXPR: If {
      condition =
      Infix {left = (Identifier { identifier = "x" }); operator = Token.LT;
        right = (Identifier { identifier = "y" })};
      consequence =
      { block = [(ExpressionStatement (Identifier { identifier = "x" }))] };
      alternative =
      (Some { block = [(ExpressionStatement (Identifier { identifier = "y" }))] })};
    ]
    Program: [
      EXPR: If {
      condition =
      Infix {left = (Identifier { identifier = "x" }); operator = Token.LT;
        right = (Identifier { identifier = "y" })};
      consequence = { block = [(Return (Identifier { identifier = "x" }))] };
      alternative =
      (Some { block = [(Return (Identifier { identifier = "y" }))] })};
    ] |}]
  ;;

  let%expect_test "function literals" =
    expect_program "fn(x, y) { return x + y; }";
    [%expect
      {|
      Program: [
        EXPR: FunctionLiteral {parameters = [{ identifier = "x" }; { identifier = "y" }];
        body =
        { block =
          [(Return
              Infix {left = (Identifier { identifier = "x" });
                operator = Token.PLUS; right = (Identifier { identifier = "y" })})
            ]
          }};
      ] |}]
  ;;

  let%expect_test "function calls" =
    expect_program "let x = add(a, b);";
    expect_program "let x = empty();";
    expect_program "let x = single(a);";
    [%expect
      {|
    Program: [
      LET: let { identifier = "x" } = Call {fn = (Identifier { identifier = "add" });
      args =
      [(Identifier { identifier = "a" }); (Identifier { identifier = "b" })]}
    ]
    Program: [
      LET: let { identifier = "x" } = Call {fn = (Identifier { identifier = "empty" }); args = []}
    ]
    Program: [
      LET: let { identifier = "x" } = Call {fn = (Identifier { identifier = "single" });
      args = [(Identifier { identifier = "a" })]}
    ] |}]
  ;;

  let%expect_test "string literals" =
    expect_program {|"hello world"|};
    [%expect {|
    Program: [
      EXPR: (String "hello world");
    ]
    |}]
  ;;

  let%expect_test "array parse" =
    expect_program "[1, 2, fn (x) { x }];";
    [%expect
      {|
      Program: [
        EXPR: (Array
         [(Integer 1); (Integer 2);
           FunctionLiteral {parameters = [{ identifier = "x" }];
             body =
             { block = [(ExpressionStatement (Identifier { identifier = "x" }))] }}
           ]);
      ] |}]
  ;;

  let%expect_test "indexing" =
    expect_program "[1, 2, 3][1 + 1];";
    [%expect
      {|
      Program: [
        EXPR: Index {left = (Array [(Integer 1); (Integer 2); (Integer 3)]);
        index =
        Infix {left = (Integer 1); operator = Token.PLUS; right = (Integer 1)}};
      ] |}]
  ;;

  let%expect_test "hash literals" =
    expect_program "{};";
    expect_program "{ 1: true, \"hello\": 17, true: false };";
    [%expect
      {|
      Program: [
        EXPR: (Hash []);
      ]
      Program: [
        EXPR: (Hash
         [((Integer 1), (Boolean true)); ((String "hello"), (Integer 17));
           ((Boolean true), (Boolean false))]);
      ] |}]
  ;;
end
