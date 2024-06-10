open Base

type t =
  { instructions : Code.instructions
  ; constants : Object.t list
  }
[@@deriving show]

type compile_error =
  { msg : string
  ; compiler : t
  }
[@@deriving show]

let init () = { instructions = []; constants = [] }
let compile compiler node = Ok compiler

module Test = struct
  type compiler_test_case =
    { input : string
    ; expected_constants : Object.t list
    ; expected_instructions : Code.instructions list
    }

  let parse program =
    let parser = Parser.init @@ Lexer.new_lex program in
    Parser.parse parser
    |> Result.map_error ~f:Parser.show_parse_error
    |> Result.ok_or_failwith
  ;;

  let test_instructions expected actual =
    let expected = List.concat expected in
    let expected_string =
      List.map expected ~f:Code.show |> String.concat ~sep:"\n"
    in
    let actual_string =
      List.map actual ~f:Code.show |> String.concat ~sep:"\n"
    in
    if not (List.length expected = List.length expected)
    then
      Fmt.failwith
        "Wrong lenth\n wanted (length: %d): %s\n Got (length: %d) %s"
        (List.length expected)
        expected_string
        (List.length actual)
        actual_string;
    List.zip_exn expected actual
    |> List.iteri ~f:(fun i (expected, actual) ->
      if not @@ Code.equal expected actual
      then
        Fmt.failwith
          "Wrong instruction at %d.\nWant=%s\nGot =%s"
          i
          (Code.show expected)
          (Code.show actual))
  ;;

  let test_constants expected actual =
    if not (List.length expected = List.length actual)
    then
      Fmt.failwith
        "Wrong number of constants. got=%d, want=%d"
        (List.length actual)
        (List.length expected);
    List.zip_exn expected actual
    |> List.iter ~f:(fun (expected, actual) ->
      if not @@ Object.equal expected actual
      then
        Fmt.failwith
          "Wrong constant.\nWant= %s\nGot = %s"
          (Object.show expected)
          (Object.show actual))
  ;;

  let run_compiler_test tests =
    List.iter
      tests
      ~f:(fun { input; expected_constants; expected_instructions } ->
        let program = parse input in
        let compiler = init () in
        match compile compiler program with
        | Ok compiler ->
          test_instructions expected_instructions compiler.instructions;
          test_constants expected_constants compiler.constants;
          ()
        | Error msg -> Fmt.failwith "%a@." pp_compile_error msg)
  ;;

  let%test "TestIntegerArithmetic" =
    let tests = [
      {
        input = "1 + 2",
        expected_constants
      }
    ]
end
