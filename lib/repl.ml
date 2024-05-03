let prompt = ">> "

let rec start in_ out =
  Out_channel.output_string out prompt;
  Out_channel.flush out;
  match In_channel.input_line in_ with
  | Some scanned ->
    let lex = Lexer.new_lex scanned in
    Lexer.print_all_tokens lex ~out;
    start in_ out
  | None -> ()
;;
