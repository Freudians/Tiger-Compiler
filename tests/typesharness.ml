open Compiler
module TigerParser = Grammar
type test_result = Success of Types.ty | Fail of string


let typecheck_file_debug filename = 
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  TigerParser.program TigerLexer.token lexbuf |> Semant.transProgDebug

let typecheck_tests filename : test_result = 
  try Success (typecheck_file_debug ("/home/anthonydu/appel/compiler/testcases/" ^ filename)) with 
 | ErrorMsg.Error msg -> (Fail msg)

let contains_str sub_str larger_str =
  let re = Str.regexp_string sub_str in
  try ignore ((Str.search_forward re larger_str 0) : int); true 
  with Not_found -> false

let rec type_to_string (typ : Types.ty) = 
  match typ with 
  | INT -> "int"
  | STRING -> "string"
  | NIL -> "nil"
  | UNIT -> "unit"
  | NAME (name, real_type) ->
      "NAME ( " ^ (Symbol.name name) ^ ", " ^ 
      (
        match !real_type with 
        | Some real_type -> type_to_string real_type
        | None -> "None"
      ) ^ ")"
  | ARRAY (element_type, _) -> "array of " ^ (type_to_string element_type)
  | RECORD (fields, _) ->
    let field_to_string ((name, typ) : Symbol.t * Types.ty) =
      (Symbol.name name) ^ " : " ^ (type_to_string typ)
    in
    let concat_field_to_string str field = 
      str ^ (field_to_string field)
    in
    let fields_to_string = 
      List.fold_left concat_field_to_string "" 
    in
    "record {" ^ (fields_to_string fields) ^ "}"

(*runs a test - returns unit if it worked, false if failed*)
let run_test filename test_result = 
  match test_result with
  | Success expected_type -> 
    begin
    match (typecheck_tests filename) with 
    Success actual_type ->
      if expected_type <> actual_type then 
        let exception_msg = 
          ("Typechecker failed on " ^ filename ^ " returning type " ^ (type_to_string actual_type)
          ^ " instead of " ^ (type_to_string expected_type)) 
        in 
        print_endline exception_msg
      else
        ()
    | Fail msg -> 
      let exception_msg = ("Typechecker failed on " ^ filename ^ " throwing exception message \n" ^
        msg ^ "\n instead of returning type \n" ^ (type_to_string expected_type)) in
      print_endline exception_msg
    end
  | Fail expected_msg ->
    begin 
      match (typecheck_tests filename) with 
      | Success actual_type ->
        let exception_msg = ("Typechecker failed on " ^ filename ^ " returning type \n" ^ 
          (type_to_string actual_type) ^ "\n instead of throwing exception message \n" ^ expected_msg) 
        in
        print_endline exception_msg
      | Fail actual_msg ->
        if not (contains_str expected_msg actual_msg) then 
          let exception_msg = ("Typechecker failed on " ^ filename ^ " throwing exception \n"
            ^ actual_msg ^ "\n Instead of \n" ^ expected_msg)
          in
          print_endline exception_msg
        else
          ()
      end
let rec run_tests (test_list : (string * test_result) list) = 
  match test_list with 
  | [] -> true 
  | (test_file, test_result) :: test_list ->
    run_test test_file test_result; run_tests test_list