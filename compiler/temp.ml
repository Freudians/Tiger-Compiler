(*Used to make new variables/temporaries and function labels*)
type t = int
type label = string
let tempCount = ref 0
(*NOTE PRETEND LIKE THERE ARE INFINITE DISTINCT REGISTERS AND THEN GO FROM THERE*)
let newTemp () = 
    tempCount := !tempCount + 1; !tempCount

(*Note that no ID can just be a number, so this is guaranteed to not conflict with
any user labels*)
let labelCount = ref 0

let newLabel () = 
    labelCount := !labelCount + 1; 
    "L" ^ string_of_int !labelCount

let label_to_string (label : label) : string = label

let temp_to_string (temp : t) : string = "R" ^ string_of_int temp