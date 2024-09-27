
let test_lex filename =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel file in
  let rec do_it () =
    let t = TigerLexer.token lexbuf in
    print_endline ((Util.token_to_string t ) ^ "   ");
    if String.length (Util.token_to_string t) = 3 && String.sub (Util.token_to_string t) 0 3 = "EOF" then
      ()
    else
      do_it ()
  in
  do_it ();
  close_in file

let () = Util.pass_cmd_arg test_lex