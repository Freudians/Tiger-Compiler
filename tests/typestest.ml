open Compiler
module TigerParser = Grammar

let typecheck_file filename = 
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  TigerParser.program TigerLexer.token lexbuf |> Semant.transProg
let typecheck_sample filename = 
  typecheck_file ("/home/anthonydu/appel/compiler/sample_programs/" ^ filename)

let assert_success filename =
  try typecheck_sample filename = () with
  | _ -> false

let%test "eq int, int" =
  (Types.(INT = INT) = true)

let%test "eq name, int" =
  (Types.((NAME (Symbol.symbol "rand", ref (Some INT))) = INT) = true)
let%test "noneq record, different record" =
  (Types.(RECORD ([], ref ()) = RECORD ([], ref())) = false)
let correct_samples = [1; 2; 3; 4; 5; 6; 7; 8; 12; 27; 30; 37; 41; 42; 44; 46; 47; 48]

let test_thrower correctSamples =
  for i = 1 to 49 do
    if List.mem i correctSamples then
      if not (assert_success ("test" ^ (string_of_int i) ^ ".tig")) then
        print_endline ("FAILED: test file " ^ (string_of_int i))
      else
        ()
    else
      if assert_success ("test" ^ (string_of_int i) ^ ".tig") then
        print_endline ("FAILED: test fie " ^ (string_of_int i))
      else
        ()
  done
let () = test_thrower correct_samples