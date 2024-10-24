type exp = unit

(**([parent], [level]) represents a function, where [parent]
is the enclosing function and [level] is its stack frame*)
type level = Frames.t * Frames.t

(**([level], [access]) represents a variable, where [level] is 
the function within which the variable is allocated and [access] is the
actual memory/register location of that variable*)
type access = level * Frames.access
let outermost = 
  let dummy = Frames.newFrame (Temp.newLabel ()) [] in 
  (dummy, dummy)
  
let cur_frame (_, curLev) = curLev

let newLevel (parent : level) lab formals = 
  let func_frame = Frames.newFrame lab (true :: formals) in
  (cur_frame parent, func_frame)

let formals (lev : level) = 
    let raw_access_wrapper raw_access = (lev, raw_access) in
    match cur_frame lev |> Frames.formals with
    | _ :: formals -> List.map raw_access_wrapper formals
    | [] -> []

let allocLocal lev isEscape =
  let var_access = Frames.allocLocal (cur_frame lev) isEscape in 
  (lev, var_access)

let print_level ((parent, frame) : level)= 
  print_endline ("Parent level: " ^ (Temp.label_to_string (Frames.name parent)));
  Frames.print_frame frame

let print_access ((enclosing, access) : access) =
  print_endline ("Within: " ^ (Temp.label_to_string (Frames.name (cur_frame enclosing))));
  print_endline (Frames.access_to_string access) 