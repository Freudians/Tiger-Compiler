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
  let frame_offset = word_size * 2 in
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

(*Tests*)
(*checks if the list of formals is correctly allocated according to escape*)
(*also verifies that the number of formals allocated is correct*)
let rec check_formals_escape access_fmls bool_fmls : bool=
  match access_fmls, bool_fmls with
  | access :: access_fmls, bool_fml :: bool_fmls ->
    begin
    match access with
    | InFrame _ -> 
        check_formals_escape access_fmls bool_fmls
    | InReg _ -> 
      if bool_fml then 
        false
      else 
        check_formals_escape access_fmls bool_fmls
    end
  | [], [] -> true
  | _, _ -> false

(*Checks if the formals are allocated correctly in memory*)
(*[check_formals_alloced expected_offset access_fmls] verifies
the formals are all allocated one after the other in memory. expected_offset
should be the value following the stored return address (normally 16)*)
let rec check_formals_alloced expected_offset access_fmls = 
  match access_fmls with
  | [] -> true
  | access :: access_fmls ->
    match access with
    | InFrame offset -> 
      if offset = expected_offset then
        check_formals_alloced (expected_offset + word_size) access_fmls
      else
        false
    | InReg _ ->
      false

(*Checks that the formals in a frame are correct*)
(*takes the input list of formals alongside the frame struct as arguments*)
let check_frame_formals frame input_formals = 
  let formals = formals frame in
  check_formals_escape formals input_formals && check_formals_alloced 16 formals

let rec construct_expected_offsets count = 
  if count = 0 then 
    []
  else
    (count * -8) :: (construct_expected_offsets (count -1))
let rec check_var_offsets expected_offsets var_accesses = 
  (*checks that each variable is 1) at a distinct location in the stack frame
  and 2) every 8-byte slot from -8 to rsp-(offset * 8) is filled
  Offsets are removed from expected_offsets if they are found so that we can check
  for duplicate offsets*)

  match var_accesses with
  | [] -> true
  | var_access :: var_accesses -> 
    match var_access with
    | InFrame offset ->
      if not (List.mem offset expected_offsets) then 
        false
      else
        let remove_val x = List.filter (fun y -> y <> x) in
        let rest_expected = remove_val offset expected_offsets in
        check_var_offsets rest_expected var_accesses
    | InReg _ -> check_var_offsets expected_offsets var_accesses
(*Checks that all the variables allocated in a frame are correct*)
let check_frame_variables ((_, count, _) : t) var_access = 
  let correct_num_var = !count = List.length var_access in
  let expected_offsets = construct_expected_offsets !count in
  correct_num_var && check_var_offsets expected_offsets var_access

let var_access_to_string var_access = 
    match var_access with 
    | InFrame offset -> "M" ^ (string_of_int offset)
    | InReg num -> "R" ^ (Temp.temp_to_string num)
let _ : string = var_access_to_string (InFrame 8)

(*test frame alone*)
let dummy_label = Temp.newLabel ()
let%test "empty frame" = 
  check_frame_formals (newFrame dummy_label []) []
(*add non-escaped frames when you implement those*)
let%test "one formal, escaped" =
  check_frame_formals (newFrame dummy_label [true]) [true]
let%test "two formals, escaped" =
  check_frame_formals (newFrame dummy_label [true; true]) [true; true]

(*now test frame with added variables*)
let dummy_frame = newFrame dummy_label [true; true; true]
let v1 = allocLocal dummy_frame true
let%test "one variable" = 
  check_frame_variables dummy_frame [v1]
let v2 = allocLocal dummy_frame true
let%test "two variables" =
  check_frame_variables dummy_frame [v2; v1]
let v3 = allocLocal dummy_frame true
let%test "three variables" = 
  check_frame_variables dummy_frame [v3; v2; v1]