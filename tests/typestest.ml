open Compiler
module TigerParser = Grammar

let typecheck_file_debug filename = 
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  TigerParser.program TigerLexer.token lexbuf |> Semant.transProgDebug
let typecheck_sample filename = 
  typecheck_file_debug ("/home/anthonydu/appel/compiler/sample_programs/" ^ filename)
let typecheck_tests filename = 
 try typecheck_file_debug ("/home/anthonydu/appel/compiler/testcases/" ^ filename) with 
 | ErrorMsg.Error msg -> print_endline msg; Types.UNIT

let%test "eq int, int" =
  (Types.(INT = INT) = true)

let%test "eq name, int" =
  (Types.((NAME (Symbol.symbol "rand", ref (Some INT))) = INT) = true)
let%test "noneq record, different record" =
  (Types.(RECORD ([], ref ()) = RECORD ([], ref())) = false)
(*Our actual unit tests*)
let%test "none" =
  typecheck_tests "none.tig" = Types.UNIT
let%test "function int base" = 
  typecheck_tests "func_int_base.tig" = Types.INT
let%test "procedure base" = 
  typecheck_tests "procedure_base.tig" = Types.UNIT
let%test "recursive base" = 
  typecheck_tests "recursive_base.tig" = Types.INT 
let%test "mutually recursive" = 
  typecheck_tests "mutually_recursive.tig" = Types.INT
let%test "multiple parameters" =
  typecheck_tests "func_multiple_param.tig" = Types.STRING
let%expect_test "undefined function" =
  let _ : Types.ty = typecheck_tests "undefined_func.tig" in ();
    [%expect {| 5.0 in  : Undefined function |}]
let%expect_test "mismatching return type" =
  let _ : Types.ty = typecheck_tests "mismatch_body.tig" in ();
  [%expect {| 2.0 in  : Mismatch between function and body type |}]
