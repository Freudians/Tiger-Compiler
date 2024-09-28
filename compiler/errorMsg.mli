val anyErrors : bool ref
val fileName : string ref
val lineNum : int ref
val linePos : int list ref
val sourceStream : in_channel ref
val error : Lexing.position -> string -> unit
exception Error
val impossible : string -> 'a   (* raises Error *)
val reset : unit -> unit
val error_no_recover : Lexing.position -> string -> 'a (* raises Error *)