(*variable in a stack frame*)
type access = InFrame of int | InReg of Temp.t

(*View shift won't be stored cuz it's the same for all amd64 functions*)
(*Frame represents a stack frame with a 3-tupel:
 (list of parameters, number of locals, and then label of function)*)
type t = access list * int ref * Temp.label

(*we will assume inf registers and then deal w/ everything later*)

(*Takes in a formals list and starting stack offset and 
returns the parsed formals list*)
(*this only really works if you assume < k params.... TODO*)
let rec parseFormals formals offset =
  match formals with
  | [] -> []
  | arg :: formals -> 
    if arg then 
      let arg_access = InFrame offset in
      arg_access :: (parseFormals formals (offset + 8))
    else 
      let arg_access = InReg (Temp.newTemp ()) in
      arg_access :: (parseFormals formals offset)

let word_size = 8
let newFrame name formals = 
  let frame_offset = word_size in
  ((parseFormals formals frame_offset, ref 0, name) : t)
let name ((_, _, label) : t) = label

let formals ((formals, _, _) : t) = formals

let allocLocal (_, count, _) isEscape =
  count := !count + 1; 
  if isEscape then
    InFrame ((!count) * -word_size)
  else
    InReg (Temp.newTemp ())

let access_to_string (access : access) = 
  match access with
  | InFrame offset -> "M" ^ (string_of_int offset)
  | InReg r -> Temp.temp_to_string r
let print_frame (frame : t) = 
  print_endline (Temp.label_to_string (name frame));
  print_endline "Formals: ";
  let print_formal () (formal : access) =
    access_to_string formal |> print_endline
  in 
  formals frame |>
  List.fold_left print_formal ();
  print_endline ("Number of arguments: " ^ (
    (let (_, num_args, _) = frame in !num_args) |> string_of_int))