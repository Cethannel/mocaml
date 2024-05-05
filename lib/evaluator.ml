open Base

let ( let* ) res f = Base.Result.bind res ~f
let ( let@ ) res f = Base.Option.bind res ~f
let map_error res = Result.map_error ~f:(fun _ -> "it failed LUL") res
let monkey_true = Object.monkey_true
let monkey_false = Object.monkey_false

let obj_is_truthy obj =
  match obj with
  | Object.Boolean res -> res
  | Null -> false
  | _ -> true
;;

let rec eval_input input =
  let lexer = Lexer.new_lex input in
  let parser = Parser.init lexer in
  let* program = Parser.parse parser |> map_error in
  let env = Environment.init () in
  eval program env

and eval node env =
  match node with
  | Ast.Program program -> eval_program program.statements env
  | Ast.Statement stmt -> eval_statement stmt env
  | _ -> Error "oh no"

and eval_program stmts env =
  List.fold_until
    stmts
    ~init:(Ok Object.Null)
    ~f:(fun _ stmt ->
      match eval_statement stmt env with
      | Ok (Object.Return obj) -> Stop (Ok obj)
      | Error _ as err -> Stop err
      | res -> Continue res)
    ~finish:(fun stmt -> stmt)

and eval_statement stmt env =
  match stmt with
  | Ast.ExpressionStatement expr -> eval_expr expr env
  | Ast.BlockStatement { block } -> eval_block block env
  | Ast.Return expr ->
    let* result = eval_expr expr env in
    Ok (Object.Return result)
  | Ast.Let { name; value } ->
    let* value = eval_expr value env in
    Environment.set env name.identifier value;
    Ok Object.Null

and eval_block block env =
  List.fold_until
    block
    ~init:(Ok Object.Null)
    ~f:(fun _ stmt ->
      match eval_statement stmt env with
      | Ok (Object.Return _ as ret) -> Stop (Ok ret)
      | Error _ as err -> Stop err
      | res -> Continue res)
    ~finish:(fun stmt -> stmt)

and eval_expr expr env =
  match expr with
  | Ast.Integer int -> Ok (Object.Integer int)
  | Ast.Boolean true -> Ok monkey_true
  | Ast.Boolean false -> Ok monkey_false
  | Ast.Identifier identifier -> eval_identifier identifier env
  | Ast.Prefix { operator = Token.BANG; right } ->
    let* right = eval_expr right env in
    eval_bang right
  | Ast.Prefix { operator = Token.MINUS; right } ->
    let* right = eval_expr right env in
    eval_minus right
  | Ast.Infix { left; operator; right } ->
    let* left = eval_expr left env in
    let* right = eval_expr right env in
    eval_infix operator left right
  | Ast.If { condition; consequence; alternative } ->
    let* condition = eval_expr condition env in
    (match condition, alternative with
     | condition, _ when obj_is_truthy condition ->
       eval_block consequence.block env
     | _, Some alternative -> eval_block alternative.block env
     | _, _ -> Ok Null)
  | Ast.FunctionLiteral { parameters; body } ->
    Ok (Object.Function { parameters; body; env })
  | Ast.Call { fn; args } ->
    let* fn = eval_expr fn env in
    let* args =
      List.fold_until
        args
        ~init:[]
        ~f:(fun accum arg ->
          match eval_expr arg env with
          | Ok arg -> Continue (arg :: accum)
          | _ -> Stop (Error "failed to eval somethin"))
        ~finish:(fun elts -> Ok (List.rev elts))
    in
    apply_function fn args
  | expr -> Fmt.error "unhandled expr: %s" (Ast.show_expression expr)

and eval_bang right =
  Ok
    (match right with
     | x when phys_equal x monkey_true -> monkey_false
     | x when phys_equal x monkey_false -> monkey_true
     | Null -> monkey_true
     | _ -> monkey_false)

and eval_minus right =
  match right with
  | Object.Integer int -> Ok (Object.Integer (-int))
  | _ -> Error "nah, you can't do that man"

and eval_infix operator left right =
  match operator, left, right with
  | _, Integer left, Integer right -> eval_integer_infix operator left right
  | Token.EQ, left, right -> Ok (Boolean (phys_equal left right))
  | Token.NOT_EQ, left, right -> Ok (Boolean (not @@ phys_equal left right))
  | _, left, right ->
    Fmt.error
      "unhandled infix: %s %s %s"
      (Object.show left)
      (Token.show operator)
      (Object.show right)

and eval_integer_infix operator left right =
  let make_int op = Ok (Object.Integer (op left right)) in
  let make_bool op = Ok (Object.Boolean (op left right)) in
  match operator with
  | Token.PLUS -> make_int ( + )
  | Token.MINUS -> make_int ( - )
  | Token.ASTERISK -> make_int ( * )
  | Token.SLASH -> make_int ( / )
  | Token.LT -> make_bool ( < )
  | Token.GT -> make_bool ( > )
  | Token.EQ -> make_bool Stdlib.( = )
  | Token.NOT_EQ -> make_bool Stdlib.( != )
  | tok -> Fmt.error "unexpected int infix op: %a" Token.pp tok

and eval_identifier identifier env =
  match Environment.get env identifier.identifier with
  | Some value -> Ok value
  | None -> Error "missing identifier"

and apply_function fn args =
  match fn with
  | Object.Function fn ->
    let env = extend_env fn args in
    let* evaluated = eval_block fn.body.block env in
    Ok (unwrap_return evaluated)
  | obj -> Fmt.error "bad function: %a" Object.pp obj

and extend_env fn args =
  List.foldi
    fn.parameters
    ~init:(Environment.enclosed fn.env)
    ~f:(fun idx env arg ->
      Environment.set env arg.identifier (List.nth_exn args idx);
      env)

and unwrap_return expr =
  match expr with
  | Return expr -> expr
  | expr -> expr
;;

module Test = struct
  open Object

  let expect_int input =
    match eval_input input with
    | Ok (Integer i) -> Fmt.pr "%d\n" i
    | Ok _ -> Fmt.pr "MISSING THE TYPE"
    | Error msg -> Fmt.failwith "%s" msg
  ;;

  let expect_bool input =
    match eval_input input with
    | Ok (Boolean b) -> Fmt.pr "%b\n" b
    | Ok expr -> Fmt.pr "WRONG TYPES: %s@." (Object.show expr)
    | Error msg -> Fmt.failwith "%s" msg
  ;;

  (*let expect_str input =
    match eval_input input with
    | Ok (String str) -> Fmt.pr "%s\n" str
    | Ok expr -> Fmt.pr "WRONG TYPES: %s@." (Object.show expr)
    | Error msg -> Fmt.failwith "%s" msg
    ;;*)

  let expect_err input =
    match eval_input input with
    | Ok expr -> Fmt.failwith "should have failed: %s@." (Object.show expr)
    | Error msg -> Fmt.pr "%s" msg
  ;;

  let%expect_test "eval integer" =
    expect_int "5;";
    expect_int "10;";
    expect_int "-10;";
    expect_int "-(-10);";
    [%expect {|
      5
      10
      -10
      10|}]
  ;;

  let%expect_test "eval bool" =
    let _ =
      expect_bool "true;";
      [%expect {| true |}]
    in
    let _ =
      expect_bool "false;";
      [%expect {| false |}]
    in
    let _ =
      expect_bool "!false;";
      [%expect {| true |}]
    in
    let _ =
      expect_bool "!!!false;";
      [%expect {| true |}]
    in
    ()
  ;;

  let%expect_test "infix expressions" =
    expect_int "2 * 5 + 5 + (3 * 5);";
    expect_int "(5 + 10 * 2 + 15 / 3) * 2 + -10;";
    expect_bool "(1 < 2) == true;";
    expect_bool "(1 > 2) != true;";
    expect_bool "(10 + 2) * 30 == 300 + 20 * 3;";
    [%expect {|
      30
      50
      false
      true
      true |}]
  ;;

  let%expect_test "if expressions" =
    expect_int "if (5 * 5 + 10 > 34) { 99 } else { 100 };";
    expect_int "if ((1000 / 2) + 250 * 2 == 1000) { 9999 };";
    [%expect {|
      99
      9999 |}]
  ;;

  let%expect_test "return statement" =
    expect_int
      {|
      if (10 > 1) {
        if (10 > 1) {
          return 10;
        }
        return 1;
      }
    |};
    [%expect {| 10 |}]
  ;;

  let%expect_test "identifiers" =
    expect_int "let a = 5; a;";
    expect_int "let a = 5; let b = a; let c = a + b + 5; c;";
    [%expect {|
      5
      15 |}]
  ;;

  let%expect_test "function literals" =
    expect_int "let identity = fn(x) { x; }; identity(5);";
    expect_int "let multiply = fn(x, y) { x * y }; multiply(50 / 2, 1 * 2);";
    expect_bool "fn(x) { x == 10 }(10);";
    [%expect {|
      5
      50
      true |}]
  ;;
end
