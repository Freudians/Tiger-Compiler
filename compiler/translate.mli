(**Stack frame*)
type level

type exp = unit

(**The location of a variable*)
type access

(*Outer level, at which globals/environments are defined*)
val outermost : level

(**[newLevel parent label formals] Generates a new level/function
enclosed by level [parent]. [formals] indicates whether or not a formal
escapes or doesn't escape*)
val newLevel : level -> Temp.label -> bool list -> level

(**[formals] is the list of arguments a function takes*)
val formals : level -> access list

(**[allocLocal level isEscape] returns an access for a variable allocated within [level]. 
[isEscape] should be true if the variable escapes*)
val allocLocal : level -> bool -> access