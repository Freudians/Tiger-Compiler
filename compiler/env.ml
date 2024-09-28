type ty = Types.ty
type enventry = VarEntry of {ty : ty} 
                | FunEntry of {formals : ty list; result : ty}
(* construct the type symbol table*)
let base_tenv_symbs = [("int", Types.INT); ("string", STRING)]
let base_venv_symbs = [ 
  ("print", FunEntry {formals = [STRING]; result = UNIT});
  ("flush", FunEntry {formals = []; result = UNIT});
  ("getchar", FunEntry {formals = []; result = STRING});
  ("ord", FunEntry {formals = [STRING]; result = INT});
  ("chr", FunEntry {formals = [INT]; result = STRING});
  ("size", FunEntry {formals = [STRING]; result = INT});
  ("substring", FunEntry {formals = [STRING; INT; INT]; result = STRING});
  ("concat", FunEntry {formals = [STRING; STRING]; result = STRING});
  ("not", FunEntry {formals = [INT]; result = INT});
  ("exit", FunEntry {formals = [INT]; result = UNIT})
]
(*converts lists of form [(s1, a1); (s2, a2); ... (sn, an)] into
symbol tables*)
let lst_to_symb (lst : (string * 'a) list) : 'a Symbol.table = 
  List.fold_left (fun x (s, ti) -> Symbol.enter x (Symbol.symbol s) ti) Symbol.empty lst
let base_tenv = 
  lst_to_symb base_tenv_symbs
let base_venv = lst_to_symb base_venv_symbs
