(** Function stack frame*)
type t 

(** Represents a variable*)
type access

(** [newFrame label formals] returns a new stack frame located at 
[label]. [formals] should indicate if each variable escapes*)
val newFrame : Temp.label -> bool list -> t

(** [name frame] returns the label corresponding to [frame]*)
val name : t -> Temp.label

(** [formals frame] returns the functino arguments in [frame] *)
val formals : t -> access list

(** [allocLocal frame isEscape] returns a local variable allocated in [frame]. 
[isEscape] should be true if the variable escapes*)
val allocLocal : t -> bool -> access 