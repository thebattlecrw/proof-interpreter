let rec repl lexbuf =
  try
    print_string "> "; flush stdout;
    Parser.main Lexer.token lexbuf;
    repl lexbuf
  with
  | End_of_file ->
      print_endline "\nBye."
  | Failure msg ->
      print_endline ("Error: " ^ msg);
      repl lexbuf
  | Parsing.Parse_error ->
      print_endline "Parse error";
      repl lexbuf
;;

let () =
  let lexbuf = Lexing.from_channel stdin in
  repl lexbuf
;;
