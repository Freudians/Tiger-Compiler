(*type access*)

(** [ty] represents types*)
type ty = Types.ty

(** [enventry] is the type of a value, variable, or function*)
type enventry = VarEntry of {ty : ty} 
                | FunEntry of {formals : ty list; result : ty}

(** [base_tenv] maps the symbols of built-in types to their types*)
val base_tenv : ty Symbol.table

(**[base_venv] maps the symbols of built-in functions to their types*)
val base_venv : enventry Symbol.table