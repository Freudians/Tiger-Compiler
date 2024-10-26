open Compiler 

let (tests : (string * Tests.Typesharness.test_result) list) = [
("intlit", Success Types.INT);
("none.tig", Success UNIT);
("func_int_base.tig", Success INT);
("procedure_base.tig", Success UNIT);
("recursive_base.tig", Success INT);
("mutually_recursive.tig", Success INT);
("func_multiple_param.tig", Success STRING);
("undefined_func.tig", Fail "Undefined function");
("mismatch_body.tig", Fail "Mismatch between function and body type")
]

let (_ : _) = Tests.Typesharness.run_tests tests