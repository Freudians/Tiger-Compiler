type ty = Types.ty
type enventry = VarEntry of {access : Translate.access; ty : ty} 
                | FunEntry of {level : Translate.level; formals : ty list; result : ty}
(* construct the type symbol table*)
let base_tenv_symbs = [("int", Types.INT); ("string", STRING)]
let base_venv_symbs = [ 
  ("print", FunEntry {level = Translate.outermost; formals = [STRING]; result = UNIT});
  ("flush", FunEntry {level = Translate.outermost;  formals = []; result = UNIT});
  ("getchar", FunEntry {level = Translate.outermost;  formals = []; result = STRING});
  ("ord", FunEntry {level = Translate.outermost;  formals = [STRING]; result = INT});
  ("chr", FunEntry {level = Translate.outermost;  formals = [INT]; result = STRING});
  ("size", FunEntry {level = Translate.outermost; formals = [STRING]; result = INT});
  ("substring", FunEntry {level = Translate.outermost; formals = [STRING; INT; INT]; result = STRING});
  ("concat", FunEntry {level = Translate.outermost; formals = [STRING; STRING]; result = STRING});
  ("not", FunEntry {level = Translate.outermost; formals = [INT]; result = INT});
  ("exit", FunEntry {level = Translate.outermost; formals = [INT]; result = UNIT})
]
(*converts lists of form [(s1, a1); (s2, a2); ... (sn, an)] into
symbol tables*)
let lst_to_symb (lst : (string * 'a) list) : 'a Symbol.table = 
  List.fold_left (fun x (s, ti) -> Symbol.enter x (Symbol.symbol s) ti) Symbol.empty lst
let base_tenv = 
  lst_to_symb base_tenv_symbs
let base_venv = lst_to_symb base_venv_symbs
