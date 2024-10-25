(*Front-end to parser*)
(*This is mainly to make testing the type-checker easier*)

let parse_file file = 
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let result = Grammar.program TigerLexer.token lexbuf in
  close_in ic; 
  result
let parse_str str = 
  let lexbuf = Lexing.from_channel str in 
  let result = Grammar.program TigerLexer.token lexbuf in 
  result

