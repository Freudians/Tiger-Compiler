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

  exception Error

  let error (pos : Lexing.position) msg =
    anyErrors := true;
    print_string !fileName;
    Printf.printf ":%d.%d" pos.pos_lnum pos.pos_bol;
    print_string ":";
    print_string msg;
    print_newline ()

  let impossible msg =
    Printf.printf "Error: Compiler bug: %s\n" msg;
    flush stdout;
    raise Error
  
  let error_no_recover pos msg = 
    error pos msg; raise Error
