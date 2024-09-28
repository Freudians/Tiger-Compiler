  (* Assume these modules are generated by ocamllex and ocamlyacc *)
  open Compiler
  module TigerParser = Grammar
  let parse filename =
    let ic = open_in filename in
      let lexbuf = Lexing.from_channel ic in
      let result = TigerParser.program TigerLexer.token lexbuf in
      close_in ic;
      Prabsyn.PrintAbsyn.print (stdout, result)    

(*    let typecheck filename = 
      let ic = open_in filename in
      let lexbuf = Lexing.from_channel ic in
      let result = Grammar.program TigerLexer.token lexbuf in
      Semant.transExp Env.base_venv Env.base_tenv result 
      *)
let () = Util.pass_cmd_arg parse