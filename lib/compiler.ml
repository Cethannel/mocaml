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
    let actual = List.concat actual in
    if List.length 

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
end
