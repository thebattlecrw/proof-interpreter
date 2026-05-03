let lexbuf = Lexing.from_channel stdin ;;

let rec repl () =
  try
    print_string "> "; flush stdout;
    Parser.main Lexer.token lexbuf;
    repl ()
  with
  | End_of_file ->
      print_endline "\nGoodbye."
  | Failure msg ->
      print_endline ("Error: " ^ msg);
      repl ()
  | Parsing.Parse_error ->
      print_endline "Parse error";
      repl ()
;;

let () = repl ()
;;
