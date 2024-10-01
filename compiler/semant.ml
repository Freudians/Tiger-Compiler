type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table
type expty = Translate.exp * Types.ty

module A = Absyn

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
          if List.equal Types.(=) formals (List.map get_exp_type args) then
            ((), result)
          else
            ErrorMsg.error_no_recover pos "Call arguments don't match"
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
              | INT, INT| STRING, STRING | ARRAY _, ARRAY _ | RECORD _, RECORD _ ->
                ((), Types.INT)
              | _ -> ErrorMsg.error_no_recover pos "Invalid operands"
            )
      )
    | RecordExp {fields; typ; pos} ->
      (
        match Symbol.look tenv typ with
        | Some real_typ ->
          (
            match real_typ with
            | Types.RECORD (formals, _) ->
              let field_to_sym (symb, exp, pos) = 
              (
                match Symbol.look tenv symb with
                | Some real_symb ->
                  (
                    if Types.(get_exp_type exp = real_symb) then
                      real_symb
                    else
                      ErrorMsg.error_no_recover pos "Mismatch between expression and type"
                  )
                | None -> ErrorMsg.error_no_recover pos "Undefined type"
              ) in
              if List.equal Types.(=) (List.map field_to_sym fields) 
              (List.map (fun (_, ty) -> ty) formals) then
                ((), real_typ)
              else 
                ErrorMsg.error_no_recover pos "Type mismatch"
            | _ -> ErrorMsg.error_no_recover pos "Type isn't a record"
          )
        | None -> ErrorMsg.error_no_recover pos "Undefined type"
      )
    | SeqExp elst -> eval_seqexp elst
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
              if Types.(then_typ = UNIT) then
                ((), Types.UNIT)
              else
                ErrorMsg.error_no_recover pos "If statements can't return a value"
          | None -> ((), then_typ)
        )
      else
        ErrorMsg.error_no_recover pos "Test isn't an int"
    | WhileExp {test; body; pos} ->
      if check_int test then
        if Types.((get_exp_type body) = UNIT) then
          ((), Types.UNIT)
        else
          ErrorMsg.error_no_recover pos "While statements can't return a value"
      else
        ErrorMsg.error_no_recover pos "Test isn't an int"
    | ForExp {var; escape=_; lo; hi; body; pos} ->
      if check_int lo then
        if check_int hi then
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
    | BreakExp _ -> ((), UNIT) (*TODO: fix*)
    | ArrayExp {typ; size; init; pos} ->
      (match Symbol.look tenv typ with
      | Some real_typ ->
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
    (*TODO: implement let*)
    | _ -> failwith "Not implemented"
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
          if other = sym then 
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
  and eval_seqexp lst =
    match lst with
    | [] -> failwith "No expression in expression sequence" 
    | [(exp, _)] -> trexp exp
    | (exp, _) :: t -> let _ = (trexp exp : expty) in eval_seqexp t
  and get_exp_type e =
    let (_, ty) = trexp e in ty
  and check_int e =
    (get_exp_type e) = Types.INT
  in
    trexp exp
and transDec (venv : venv) (tenv : tenv) (dec: A.dec) =
  let transFunc params body : Env.enventry = 
    let enter_field (venv_ : venv) ({name; escape=_; typ; pos} : A.field) = 
      (
        match Symbol.look tenv typ with
        | Some real_typ -> Symbol.enter venv_ name (Env.VarEntry{ty=real_typ})
        | None -> ErrorMsg.error_no_recover pos "Undefined type"
      ) in
    let body_venv = List.fold_left enter_field venv params in
    let (_, res_typ) = transExp body_venv tenv body in
    Env.FunEntry{formals=
      List.map (fun ({name=_; escape=_; typ; pos} : A.field) -> 
        (
          match Symbol.look tenv typ with
          | Some real_typ -> real_typ
          | None -> ErrorMsg.error_no_recover pos "Undefined type"
        )) params; result=res_typ}
  in
  let enterFunc venv_ ({ name; params; result; body; pos } : A.fundec) =
    (match result with
    | Some (eresult, res_pos) -> 
      (match Symbol.look tenv eresult with
      | Some real_eresult -> 
        (
          match transFunc params body with
          |Env.FunEntry{formals;result=o_result} ->
            if Types.(o_result = real_eresult) then
              Symbol.enter venv_ name (Env.FunEntry{formals=formals;result=o_result})
            else 
              ErrorMsg.error_no_recover res_pos "Mismatching types"
          | _ -> ErrorMsg.error_no_recover pos "Something went serioiusly wrong"    
        )
      | None -> ErrorMsg.error_no_recover res_pos "Undefined type")
    | None -> 
      Symbol.enter venv_ name (transFunc params body)
    )
  in
  match dec with
  | A.VarDec {name; escape=_; typ; init; _} ->
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
        (Symbol.enter venv name (Env.VarEntry{ty=actual_typ}), tenv))
  | A.TypeDec [{name; ty; _}] ->
    (venv, Symbol.enter tenv name (transTy tenv ty))   
  | A.TypeDec tlst ->
    let enter_type tenv_ ({name; ty; _} : A.atypedec) =
      Symbol.enter tenv_ name (transTy tenv_ ty)
    in
    (venv, List.fold_left enter_type tenv tlst)
  | A.FunctionDec [fdec] ->
    (enterFunc venv fdec, tenv)
  | A.FunctionDec flst ->
    (List.fold_left enterFunc venv flst, tenv)

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