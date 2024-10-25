  let anyErrors = ref false
  let fileName = ref ""
  let lineNum = ref 1
  let linePos = ref [1]
  let sourceStream = ref stdin

  let reset () =
    anyErrors := false;
    fileName := "";
    lineNum := 1;
    linePos := [1];
    sourceStream := stdin

  exception Error of string

  let error (pos : Lexing.position) msg =
    anyErrors := true;
    Printf.sprintf "%d.%d in %s : %s" pos.pos_lnum pos.pos_bol !fileName msg

  let impossible msg =
    Printf.printf "Error: Compiler bug: %s\n" msg;
    flush stdout;
    raise (Error msg)
  
  let error_no_recover pos msg = 
    raise (Error (error pos msg))
