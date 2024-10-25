(*TODO: add support for control sequences*)
{
    let counter = ref 0
    let string_buff = Buffer.create 256
    open Grammar
    let next_line (lexbuf : Lexing.lexbuf) =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    { pos with 
               pos_lnum = pos.pos_lnum + 1;
               pos_cnum = 0;
    }
    let add_col (_ : Lexing.lexbuf) _ : unit = ()
    let inc_col (lexbuf : Lexing.lexbuf) = add_col lexbuf 1
}
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let weird_whitespace = [' ''\x0C''\n''\r''\t']
let newline = '\n'
(* Note: because ocaml thinks identifiers starting with capital letters = module or variant, all identifiers
have been replaced from all caps -> identifier ^ "_" *)
rule token = parse
(*whitespace*)
| newline {next_line lexbuf; token lexbuf}
| (' '|'\t')+ {token lexbuf}
| "/*" {counter := 1; comments lexbuf}
| "\"" {Buffer.clear string_buff; strings lexbuf; STRING (Buffer.contents string_buff)}
| "while" { add_col lexbuf 5; WHILE}
| "for" {add_col lexbuf 3; FOR}
| "to" {add_col lexbuf 2; TO}
| "break" {add_col lexbuf 5; BREAK}
| "let" {add_col lexbuf 3; LET}
| "in" {add_col lexbuf 2; IN}
| "end" {add_col lexbuf 3; END}
| "function" {add_col lexbuf 8; FUNCTION}
| "var" {add_col lexbuf 3; VAR}
| "type" {add_col lexbuf 4; TYPE}
| "array" {add_col lexbuf 5; ARRAY}
| "if" {add_col lexbuf 2; IF}
| "then" {add_col lexbuf 4; THEN}
| "else" {add_col lexbuf 4; ELSE}
| "do" {add_col lexbuf 2; DO}
| "of" {add_col lexbuf 2; OF}
| "nil" {add_col lexbuf 3; NIL}
| ":=" {add_col lexbuf 2; ASSIGN}
| "," {add_col lexbuf 1; COMMA}
| ":" {add_col lexbuf 1; COLON}
| ";" {add_col lexbuf 1; SEMICOLON}
| "." {add_col lexbuf 1; DOT}
| "(" {add_col lexbuf 1; LPAREN}
| ")" {inc_col lexbuf; RPAREN}
| "{" {inc_col lexbuf; LBRACE}
| "}" {inc_col lexbuf;RBRACE}
| "[" {inc_col lexbuf;LBRACK}
| "]" {inc_col lexbuf;RBRACK}
| "&" {inc_col lexbuf;AND}
| "|" {inc_col lexbuf;OR}
| "*/" {ErrorMsg.error_no_recover (Lexing.lexeme_start_p lexbuf) "Unclosed comment from back";}
| "/" {inc_col lexbuf;DIVIDE}
| "*" {inc_col lexbuf;TIMES}
| "+" {inc_col lexbuf;PLUS}
| "-" {inc_col lexbuf;MINUS}
| "<>" {add_col lexbuf 2;NEQ}
| "<=" {add_col lexbuf 2; LE}
| ">=" {add_col lexbuf 2;GE}
| ">" {add_col lexbuf 2;GT}
| "<" {add_col lexbuf 2;LT}
| "=" {add_col lexbuf 2;EQ}
(*identifiers*)
| upper|lower (digit|upper|lower|'_')* {add_col lexbuf ((Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf)); 
            ID (Lexing.lexeme lexbuf)}
(*Integer literals*)
| digit+ {add_col lexbuf (Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf);
                    INT (int_of_string (Lexing.lexeme lexbuf))}
| eof {EOF}
and comments = parse
| "/*" {counter := !counter + 1; add_col lexbuf 2; comments lexbuf}
| "*/" {counter := !counter -1; 
        if !counter > 0 then 
                (add_col lexbuf 2; 
                comments lexbuf)
        else 
        
            if !counter = 0 then 
                (add_col lexbuf 2; token lexbuf)
            else 
            (ErrorMsg.error_no_recover (Lexing.lexeme_start_p lexbuf) "unclosed comment from back")}
| newline {next_line lexbuf; comments lexbuf}
| _ {add_col lexbuf ((Lexing.lexeme_end lexbuf - Lexing.lexeme_start lexbuf));
            comments lexbuf}
| eof {ErrorMsg.error_no_recover (Lexing.lexeme_start_p lexbuf) "unclosed comment"}
and strings = parse 
| "\\t" {Buffer.add_char string_buff '\t'; add_col lexbuf 2; strings lexbuf}
| "\\n" {Buffer.add_char string_buff '\n'; add_col lexbuf 2; strings lexbuf}
| "\\\"" {Buffer.add_char string_buff '\"'; add_col lexbuf 2; strings lexbuf}
| "\\\\" {Buffer.add_char string_buff '\\'; add_col lexbuf 2; strings lexbuf}
| "\\" digit digit digit {add_col lexbuf 4;
                Buffer.add_char string_buff ((Lexing.lexeme lexbuf) |> (fun s -> String.sub s 1 3) |> int_of_string |> Char.chr); strings lexbuf}
| "\\" weird_whitespace {inc_col lexbuf; ignore_string lexbuf}
| "\"" {inc_col lexbuf; ()}
| newline {next_line lexbuf; Buffer.add_char string_buff '\n'; strings lexbuf}
| [^'\\'] as c {inc_col lexbuf; Buffer.add_char string_buff c; strings lexbuf}
| eof {ErrorMsg.error_no_recover (Lexing.lexeme_start_p lexbuf) "unclosed string"}
| _ {ErrorMsg.error_no_recover (Lexing.lexeme_start_p lexbuf) "illegal character in string"}
and ignore_string = parse
| weird_whitespace "\\" {strings lexbuf}
| _ {ignore_string lexbuf}
| eof {ErrorMsg.error_no_recover (Lexing.lexeme_start_p lexbuf) "unclosed string ignore"}
