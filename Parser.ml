  (* Assume these modules are generated by ocamllex and ocamlyacc *)
  open Compiler
  module TigerParser = Grammar
  let parse filename =
    let ab_syn_tree = Parser.parse_file filename in 
    Prabsyn.PrintAbsyn.print (stdout, ab_syn_tree)
(*    let typecheck filename = 
      let ic = open_in filename in
      let lexbuf = Lexing.from_channel ic in
      let result = Grammar.program TigerLexer.token lexbuf in
      Semant.transExp Env.base_venv Env.base_tenv result 
      *)
let () = Util.pass_cmd_arg parse