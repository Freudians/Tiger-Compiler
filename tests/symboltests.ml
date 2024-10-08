open Compiler

let%test "symbol symbol" = 
  Symbol.symbol "hi" |> Symbol.name = "hi"
let%test "two differing symbols" = 
  let sy1 = Symbol.symbol "hi" in
  let sy2 = Symbol.symbol "hi" in
  let ta = Symbol.enter Symbol.empty sy1 9 in
  Symbol.look ta sy2 = Some 9
let%test "None where no symbol" =
  Symbol.look Symbol.empty (Symbol.symbol "int") = None
let%test "Multiple symbols" = 
  let sy1 = Symbol.symbol "bye" in
  let sy2 = Symbol.symbol "lie" in
  let ta = Symbol.enter (Symbol.enter (Symbol.empty) sy1 3) sy2 4 in
  Symbol.look ta sy1 = Some 3