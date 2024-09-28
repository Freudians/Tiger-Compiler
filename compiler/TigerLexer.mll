(*TODO: add support for control sequences*)
{
    let counter = ref 0
    let string_buff = Buffer.create 256
    open Grammar
}
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let weird_whitespace = [' ''\x0C''\n''\r''\t']
(* Note: because ocaml thinks identifiers starting with capital letters = module or variant, all identifiers
have been replaced from all caps -> identifier ^ "_" *)
rule token = parse
(*whitespace*)
| (' '|'\n'|'\t')+ {token lexbuf}
| "/*" {counter := 1; comments lexbuf}
| "\"" {Buffer.clear string_buff; strings lexbuf; STRING (Buffer.contents string_buff)}
| "while" { WHILE}
| "for" {FOR}
| "to" {TO}
| "break" {BREAK}
| "let" {LET}
| "in" {IN}
| "end" {END}
| "function" {FUNCTION}
| "var" {VAR}
| "type" {TYPE}
| "array" {ARRAY}
| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "do" {DO}
| "of" {OF}
| "nil" {NIL}
| ":=" {ASSIGN}
| "," {COMMA}
| ":" {COLON}
| ";" {SEMICOLON}
| "." {DOT}
| "(" {LPAREN}
| ")" {RPAREN}
| "{" {LBRACE}
| "}" {RBRACE}
| "[" {LBRACK}
| "]" {RBRACK}
| "&" {AND}
| "|" {OR}
| "*/" {ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "Unclosed comment from back"; EOF}
| "/" {DIVIDE}
| "*" {TIMES}
| "+" {PLUS}
| "-" {MINUS}
| "<>" {NEQ}
| "<=" {LE}
| ">=" {GE}
| ">" {GT}
| "<" {LT}
| "=" {EQ}
(*identifiers*)
| upper|lower (digit|upper|lower|'_')* {ID (Lexing.lexeme lexbuf)}
(*Integer literals*)
| digit+ {INT (int_of_string (Lexing.lexeme lexbuf))}
| eof {EOF}
and comments = parse
| "/*" {counter := !counter + 1; comments lexbuf}
| "*/" {counter := !counter -1; if !counter > 0 then comments lexbuf else if !counter = 0 then token lexbuf else 
        (ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "unclosed comment from back"; EOF)}
| _ {comments lexbuf}
| eof {ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "unclosed comment"; EOF}
and strings = parse 
| "\\t" {Buffer.add_char string_buff '\t'; strings lexbuf}
| "\\n" {Buffer.add_char string_buff '\n'; strings lexbuf}
| "\\\"" {Buffer.add_char string_buff '\"'; strings lexbuf}
| "\\\\" {Buffer.add_char string_buff '\\'; strings lexbuf}
| "\\" digit digit digit {Buffer.add_char string_buff ((Lexing.lexeme lexbuf) |> (fun s -> String.sub s 1 3) |> int_of_string |> Char.chr); strings lexbuf}
| "\\" weird_whitespace {ignore_string lexbuf}
| "\"" {()}
| [^'\\'] as c {Buffer.add_char string_buff c; strings lexbuf}
| eof {ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "unclosed string";}
| _ {ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "illegal character in string";}
and ignore_string = parse
| weird_whitespace "\\" {strings lexbuf}
| _ {ignore_string lexbuf}
| eof {ErrorMsg.error (Lexing.lexeme_start_p lexbuf) "unclosed string ignore";}
