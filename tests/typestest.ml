open Compiler

let%test "eq int, int" =
  (Types.(INT = INT) = true)

let%test "eq name, int" =
  (Types.((NAME (Symbol.symbol "rand", ref (Some INT))) = INT) = true)
let%test "noneq record, different record" =
  (Types.(RECORD ([], ref ()) = RECORD ([], ref())) = false)