type t
type label 

(*Produces a unique temporary to represent a value held in a register*)
val newTemp : unit -> t

(*Produces a code label that is unused by any other function*)
val newLabel : unit -> label