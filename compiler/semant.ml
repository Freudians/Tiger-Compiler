type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table
type expty = Translate.exp * Types.ty

module A = Absyn
let loopstack = Stack.create ()

let get_type (tenv : tenv) (sym : Symbol.t) pos : Types.ty =
  match Symbol.look tenv sym with
  | Some typ -> typ
  | None -> ErrorMsg.error_no_recover pos "Undefined type"
let rec transExp (venv : venv) (tenv : tenv) (exp : A.exp) : expty =
  let rec trexp exp =
    match exp with
    | A.VarExp var -> trvar var
    | NilExp -> ((), Types.NIL)
    | IntExp _ -> ((), Types.INT)
    | StringExp _ -> ((), Types.STRING)
    | CallExp {func; args; pos} ->
      begin
      match (Symbol.look venv func) with
      | Some real_func -> 
        begin
        match real_func with
        | Env.FunEntry {formals; result} ->
          Stack.push 0 loopstack;
          if List.equal Types.(=) formals (List.map get_exp_type args) then
            ((), result)
          else
            ErrorMsg.error_no_recover pos "Bad type in function call args"
        | Env.VarEntry _ ->
          ErrorMsg.error_no_recover pos "Expected function"
        end
      | None -> ErrorMsg.error_no_recover pos "Undefined function"
      end
    | OpExp {left; oper; right; pos} ->
      (
        match oper with
        | PlusOp | MinusOp | TimesOp | DivideOp ->
          (
            if (check_int left) && (check_int right) then
              ((), Types.INT)
            else
              ErrorMsg.error_no_recover pos "Non-integer operands"
          )
          | LtOp | LeOp | GtOp | GeOp ->
          (
            if ((check_int left) && (check_int right)) ||
              ((get_exp_type left) = Types.STRING && (get_exp_type right) = Types.STRING) then
                ((), Types.INT)
          else
            ErrorMsg.error_no_recover pos "Operands must either both be integers or strings"
          )
          | EqOp | NeqOp -> 
            (match ((get_exp_type left), (get_exp_type right)) with
              | INT, INT| STRING, STRING | ARRAY _, ARRAY _ | RECORD _, RECORD _ 
              | RECORD _, NIL | NIL, RECORD _ ->
                ((), Types.INT)
              | _ -> ErrorMsg.error_no_recover pos "Invalid operands"
            )
      )
    | RecordExp {fields; typ; pos} ->
      (
        match Symbol.look tenv typ with
        | Some wrapped_real_typ ->
          (
            let real_typ = Types.actual_ty wrapped_real_typ in
            match real_typ with
            | Types.RECORD (formals, _) ->
              let field_sym_eq (symb, ty) (esymb, ety) =
                ((Symbol.name symb) = (Symbol.name esymb)) &&
                Types.(ty = ety)
              in
              if List.equal field_sym_eq 
              (List.map (fun (sym, exp, _) -> (sym, get_exp_type exp)) fields)
              formals then
                ((), real_typ)
              else 
                ErrorMsg.error_no_recover pos "Type or name mismatch in record"
            | _ -> ErrorMsg.error_no_recover pos "Type isn't a record"
          )
        | None -> ErrorMsg.error_no_recover pos "Undefined type"
      )
    | SeqExp elst -> 
      let rec eval_seqexp lst =
        match lst with
        | [] -> ((), Types.UNIT) (*Very, very ugly hack to handle lack of UnitExp*)
        | [(exp, _)] -> trexp exp
        | (exp, _) :: t -> let _ = (trexp exp : expty) in eval_seqexp t    
      in
      eval_seqexp elst
    | AssignExp {var; exp; pos} ->
      let (_, var_typ) = trvar var in
      let exp_typ = get_exp_type exp in
      if Types.(var_typ = exp_typ) then
        ((), Types.NIL)
      else
        ErrorMsg.error_no_recover pos "Mismatch between variable and expression type"
    | IfExp {test; then_; else_; pos} ->
      if check_int test then
        (
          let then_typ = get_exp_type then_ in
          match else_ with
          | Some real_else ->
            if Types.(then_typ = (get_exp_type real_else)) then
              ((), then_typ)
            else
              ErrorMsg.error_no_recover pos "then and else body type mismatch"
          | None -> 
            if Types.(then_typ = UNIT) then
              ((), Types.UNIT)
            else
              ErrorMsg.error_no_recover pos "then statement cant' returna value"
        )
      else
        ErrorMsg.error_no_recover pos "Test isn't an int"
    | WhileExp {test; body; pos} ->
      if check_int test then
        let loopcount = (Stack.pop loopstack) + 1 in Stack.push loopcount loopstack;
        if Types.((get_exp_type body) = UNIT) then
          ((), Types.UNIT)
        else
          ErrorMsg.error_no_recover pos "While statements can't return a value"
      else
        ErrorMsg.error_no_recover pos "Test isn't an int"
    | ForExp {var; escape=_; lo; hi; body; pos} ->
      if check_int lo then
        if check_int hi then
          let loopcount = (Stack.pop loopstack) + 1 in Stack.push loopcount loopstack;
          let (_, exptyp) = 
          transExp (Symbol.enter venv var (Env.VarEntry{ty=Types.INT})) tenv body in
            if Types.(exptyp = UNIT) then
              ((), Types.UNIT)
            else
              ErrorMsg.error_no_recover pos "For statement body must return unit"
        else
          ErrorMsg.error_no_recover pos "Hi of for statement must be int"
      else
        ErrorMsg.error_no_recover pos "lo of for statement must return int"
    | BreakExp pos -> 
      let loopcount = Stack.pop loopstack in
      if loopcount = 0 then
        ErrorMsg.error_no_recover pos "Break statement not in whie/for loop"
      else
        Stack.push (loopcount -1) loopstack;
        ((), UNIT) (*TODO: fix*)
    | ArrayExp {typ; size; init; pos} ->
      (match Symbol.look tenv typ with
      | Some wrapped_real_typ ->
        let real_typ = Types.actual_ty wrapped_real_typ in
        (match real_typ with
          | Types.ARRAY (arrty, _) ->
            if check_int size then
              if Types.((get_exp_type init) = arrty) then
                ((), real_typ)
              else
                ErrorMsg.error_no_recover pos "Init type must match array element type"
            else
              ErrorMsg.error_no_recover pos "Size must be int type"
          | _ -> ErrorMsg.error_no_recover pos "Type must be an array type"
        )
      | None -> ErrorMsg.error_no_recover pos "Undefined type")
    | LetExp{decs; body; _} ->
      let (venv_, tenv_) = 
      List.fold_left (fun (svenv, stenv) dec -> transDec svenv stenv dec) (venv, tenv) decs
      in
      transExp venv_ tenv_ body
  and trvar var =
    match var with
    | SimpleVar (sym, pos) ->
      (match Symbol.look venv sym with
      | Some real_sym ->
        (match real_sym with
        | VarEntry{ty} -> ((), Types.actual_ty ty)
        | _ -> ErrorMsg.error_no_recover pos "Variable expected but got function")
      | None -> ErrorMsg.error_no_recover pos "Undefined variable")
    | FieldVar (var, sym, pos) ->
      (*TODO: GACK! refactor*)
      let (_, varty) = trvar var in
      (match varty with
      | Types.RECORD (flst, _) -> 
        let rec get_ftyp fieldlst =
        (match fieldlst with
        | (other, other_ty) :: t -> 
          if (Symbol.name other) = (Symbol.name sym) then 
            ((), Types.actual_ty other_ty) 
          else 
            get_ftyp t
        | [] -> ErrorMsg.error_no_recover pos "Field doesn't exist in record")
        in
        get_ftyp flst
      | _ -> ErrorMsg.error_no_recover pos "Variable isn't a record")
    | SubscriptVar (var, exp, pos) ->
      let (_, varty) = trvar var in
      (match varty with
      | Types.ARRAY (arrty, _) -> 
        if check_int exp then
          ((), Types.actual_ty arrty)
        else
        ErrorMsg.error_no_recover pos "Array subscript not an integer"
      | _ -> ErrorMsg.error_no_recover pos "Subscript used on non-array type")
  and get_exp_type e =
    let (_, ty) = trexp e in ty
  and check_int e =
    (get_exp_type e) = Types.INT
  in
    trexp exp
and transDec (venv : venv) (tenv : tenv) (dec: A.dec) =
  let transParams params = 
      List.map (fun ({name=_; escape=_; typ; pos} : A.field) -> 
        (
          match Symbol.look tenv typ with
          | Some real_typ -> real_typ
          | None -> ErrorMsg.error_no_recover pos "Undefined type"
        )) params
  in
  let enterFuncHelper rvenv expected_typ ({ name; params; result=_; body; pos } : A.fundec) =
    let fvenv = List.fold_left (fun venv_ ({name; escape=_; typ; pos} : A.field) -> 
      match Symbol.look tenv typ with
      | Some real_typ -> Symbol.enter venv_ name (Env.VarEntry{ty=real_typ})
      | None -> ErrorMsg.error_no_recover pos "Undefined type") rvenv params
    in
    let (_, bodytyp) = transExp fvenv tenv body in
    if Types.(bodytyp = expected_typ) then
      let fparams = transParams params in
      (Symbol.enter rvenv name (Env.FunEntry{result=Types.UNIT;formals=fparams}))
    else 
      ErrorMsg.error_no_recover pos "Type of a procedure must be UNIT"
  in
  let enterFunc rvenv (fdec : A.fundec) =
    match fdec.result with
    | Some (expectedtyp, pos) ->( 
      match Symbol.look tenv expectedtyp with
      | Some real_typ -> enterFuncHelper rvenv real_typ fdec
      | None -> ErrorMsg.error_no_recover pos "Undefined type"
    )
    | None -> enterFuncHelper rvenv Types.UNIT fdec
  in
  match dec with
  | A.VarDec {name; escape=_; typ; init; pos} ->
      (match typ with
      | Some (expected_typ, pos) ->
        (
          match Symbol.look tenv expected_typ with 
          | Some real_expected_typ ->
            let (_, actual_typ) = transExp venv tenv init in
            if Types.(real_expected_typ = actual_typ) then
              ((Symbol.enter venv name (Env.VarEntry{ty=real_expected_typ})), tenv)
            else
              ErrorMsg.error_no_recover pos "Mismatched type in variable declaration"
          | None -> ErrorMsg.error_no_recover pos "Undefined type"
        )
      | None -> let (_, actual_typ) = transExp venv tenv init in 
        if actual_typ = Types.NIL then
          ErrorMsg.error_no_recover pos "Variable initialized with nil must be type marker"
        else
          (Symbol.enter venv name (Env.VarEntry{ty=actual_typ}), tenv))
  | A.TypeDec tlst ->
    let enter_type_header tenv_ ({name; ty=_; pos=_} : A.atypedec) = 
      Symbol.enter tenv_ name (Types.NAME (name, ref None))
    in
    let enter_type tenv_ ({name; ty; _} : A.atypedec) =
      match Symbol.look tenv_ name with
      | Some (Types.NAME (_, actual_type)) -> actual_type := (Some (transTy tenv_ ty));
        tenv_
      | Some _ | None-> Symbol.enter tenv_ name (transTy tenv_ ty)
    in
    (*returns true if there is a name cycle, false otherwise*)
    let rec check_graph_cycle encountered_names first_chain =
      match first_chain with
      | Types.NAME (other_name, storedty) ->
        if List.mem (Symbol.name other_name) encountered_names then
          true
        else
          (match !storedty with
          | Some realstoredty -> check_graph_cycle ((Symbol.name other_name) :: encountered_names) realstoredty
          | None -> false)
      | _ -> false
    in
    (*returns true if there is a simple cycle, false otherwise*)
    let rec cycle_check tenv_ (added_types : A.atypedec list) = 
      match added_types with
      | [] -> ()
      | ({name=starting_name; ty=ty; pos=pos}) :: t ->
        if check_graph_cycle [(Symbol.name starting_name)] (transTy tenv_ ty) then
          ErrorMsg.error_no_recover pos "Simple cycle detected in type declaration!"
        else 
          cycle_check tenv_ t
    in
    (* throws exception if types with same name are found, otherwise unit*)
    (*tenv_ : type environment with added types*)
    (*tlst_ : list of symbols that were added*)
    let rec dupli_name_check (tlst_ : A.atypedec list) alr_decl =
      match tlst_ with
      | [] -> ()
      | ({name; ty=_; pos;}) :: t ->
        if List.mem name alr_decl then 
          ErrorMsg.error_no_recover pos "Same name as previous type in recursive declaration"
        else
          dupli_name_check t (name :: alr_decl)
    in
    dupli_name_check tlst []; 
    let header_tenv = List.fold_left enter_type_header tenv tlst in
    let result_tenv = List.fold_left enter_type header_tenv tlst in
    cycle_check result_tenv tlst;
    ( venv, result_tenv)
  | A.FunctionDec flst ->
    let enterFuncHeader venv_ ({ name; params; result; body=_; pos=_ } : A.fundec) = 
      match result with
      | Some (resSymb, pos) ->
        (match Symbol.look tenv resSymb with
        | Some realResSymb -> Symbol.enter venv_ name (Env.FunEntry{formals=transParams params; result=realResSymb})
        | None -> ErrorMsg.error_no_recover pos "Undefined type")
      | None -> Symbol.enter venv_ name (Env.FunEntry{formals=transParams params; result=Types.UNIT})
    in
    (*throws exception of functions with same name are found, otherwise unit*)
    let rec check_same_name_helper (flst_ : A.fundec list) alr_decl = 
      match flst_ with
      | [] -> ()
      | ({name; params=_; result=_; body=_; pos;} : A.fundec) :: ft ->
        if List.mem name alr_decl then
          ErrorMsg.error_no_recover pos "Function has same name as previous one in recursive block"
        else
          check_same_name_helper ft (name :: alr_decl)
    in
    let check_same_name flst_ = check_same_name_helper flst_ [] in
    check_same_name flst; 
    let recursiveVenv = List.fold_left enterFuncHeader venv flst in
    (List.fold_left enterFunc recursiveVenv flst, tenv)

and transTy (tenv : tenv) (typ : A.ty) = 
  match typ with
  | NameTy (sym, pos) ->
    (match Symbol.look tenv sym with
    | Some real_typ -> (Types.actual_ty real_typ)
    | None -> ErrorMsg.error_no_recover pos "undefined type")
  | RecordTy flst ->
    let fieldToSym ({name; escape=_; typ; pos} : A.field) =
      match Symbol.look tenv typ with
      | Some real_typ -> (name, real_typ)
      | None -> ErrorMsg.error_no_recover pos "Undefined type"
    in
      Types.RECORD (List.map fieldToSym flst, ref ())
  | ArrayTy (sym, pos) ->
    match Symbol.look tenv sym with
    | Some real_typ -> Types.ARRAY (real_typ, ref())
    | None -> ErrorMsg.error_no_recover pos "Undefined type"

let transProg exp =
  Stack.push 0 loopstack;
  let (_, _) = transExp Env.base_venv Env.base_tenv exp in
  ()