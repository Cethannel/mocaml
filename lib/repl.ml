let prompt = ">> "

let rec start in_ out =
  Out_channel.output_string out prompt;
  Out_channel.flush out;
  let env = Environment.init () in
  match In_channel.input_line in_ with
  | Some scanned ->
    let lex = Lexer.new_lex scanned in
    let parser = Parser.init lex in
    let program = Parser.parse parser in
    let _ =
      match program with
      | Error msg -> Fmt.pr "%a@." Parser.pp_parse_error msg
      | Ok program ->
        let str =
          match Evaluator.eval program env with
          | Error msg -> msg
          | Ok out -> Object.show out
        in
        Out_channel.output_string out str
    in
    start in_ out
  | None -> ()
;;
