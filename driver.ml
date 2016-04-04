open Types

let _ =
  let lexbuf = Lexing.from_channel stdin in
    while true do
      output_string stdout ">> ";
      flush stdout;
      try
        let result = Parser.main Lexer.token lexbuf in
          result |> desugar |> evaluate
                 |> valToString |> output_string stdout;
          print_newline();
      with
        | Parsing.Parse_error       -> output_string stdout "Parse error in statement";
                                       print_newline();
                                       Lexing.flush_input lexbuf
        | Lexer.Eof                 -> output_string stdout "Exit code received";
                                       print_newline();
                                       exit 0
        | Lexer.Unrecognized        -> output_string stdout "Unrecognized token error";
                                       print_newline();
                                       Lexing.flush_input lexbuf
        | Desugar s                 -> output_string stdout ("Desugar error: " ^ s);
                                       print_newline();
                                       Lexing.flush_input lexbuf
        | Interp s                  -> output_string stdout ("Interpret error: " ^ s);
                                       print_newline();
                                       Lexing.flush_input lexbuf
    done
