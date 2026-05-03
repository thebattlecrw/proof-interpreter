let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    Parser.main Lexer.token lexbuf
  with
  | Failure msg -> print_endline ("Error: " ^ msg)
  | Parsing.Parse_error -> print_endline "Parse error"
;;
