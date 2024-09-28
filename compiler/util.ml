module TigerParser = Grammar
(*A bunch of util functions*)
let token_to_string token =
  match token with
  | TigerParser.WHILE -> "WHILE"
  | TigerParser.FOR -> "FOR"
  | TigerParser.TO -> "TO"
  | TigerParser.BREAK -> "BREAK"
  | TigerParser.LET -> "LET"
  | TigerParser.IN -> "IN"
  | TigerParser.END -> "END"
  | TigerParser.FUNCTION -> "FUNCTION"
  | TigerParser.VAR -> "VAR"
  | TigerParser.TYPE -> "TYPE"
  | TigerParser.ARRAY -> "ARRAY"
  | TigerParser.IF -> "IF"
  | TigerParser.THEN -> "THEN"
  | TigerParser.ELSE -> "ELSE"
  | TigerParser.DO -> "DO"
  | TigerParser.OF -> "OF"
  | TigerParser.NIL -> "NIL"
  | TigerParser.ASSIGN -> "ASSIGN"
  | TigerParser.COMMA -> "COMMA"
  | TigerParser.COLON -> "COLON"
  | TigerParser.SEMICOLON -> "SEMICOLON"
  | TigerParser.DOT -> "DOT"
  | TigerParser.LPAREN -> "LPAREN"
  | TigerParser.RPAREN -> "RPAREN"
  | TigerParser.LBRACE -> "LBRACE"
  | TigerParser.RBRACE -> "RBRACE"
  | TigerParser.LBRACK -> "LBRACK"
  | TigerParser.RBRACK -> "RBRACK"
  | TigerParser.AND -> "AND"
  | TigerParser.OR -> "OR"
  | TigerParser.DIVIDE -> "DIVIDE"
  | TigerParser.TIMES -> "TIMES"
  | TigerParser.PLUS -> "PLUS"
  | TigerParser.MINUS -> "MINUS"
  | TigerParser.NEQ -> "NEQ"
  | TigerParser.LE -> "LE"
  | TigerParser.GE -> "GE"
  | TigerParser.GT -> "GT"
  | TigerParser.LT -> "LT"
  | TigerParser.EQ -> "EQ"
  | TigerParser.ID s -> "ID(" ^ s ^ ")"
  | TigerParser.INT i -> "INT(" ^ string_of_int i ^ ")"
  | TigerParser.STRING s -> "STRING(\"" ^ s ^ "\")"
  | TigerParser.EOF -> "EOF"

(* A very common pattern is to apply a single function to a file name*)
(* this takes a function that takes a single file name as an argument,
  and supplies a command line arg to it*)

let pass_cmd_arg (f : string -> unit) : unit =
  let input_file = ref [] in
  let () = Arg.parse [] (fun f -> input_file := f :: !input_file) ""
  in
  if (List.length !input_file) > 1 then
    print_endline "Too many files given"
  else
  match !input_file with
  | input_f :: _ -> f input_f
  | [] -> print_endline "Must provide a file"