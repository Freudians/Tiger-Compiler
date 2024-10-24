type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table
(*Handle break statements: *)
type break_signal = Break | Continue
type expty = Translate.exp * Types.ty * break_signal


module A = Absyn

let type_eq = Types.(=)

let same_type t1 t2 = 
  type_eq t1 t2

let both_type t1 t2 typ = 
  Types.(t1 = typ) && Types.(t1 = t2)
  
let get_type (tenv : tenv) (sym : Symbol.t) pos : Types.ty =
  match Symbol.look tenv sym with
  | Some typ -> typ
  | None -> ErrorMsg.error_no_recover pos "Undefined type"
let rec transExp (venv : venv) (tenv : tenv) (exp : A.exp) (level : Translate.level): expty =
  let rec trexp exp =
    match exp with
    | A.VarExp var -> let (translated_exp, var_type) = trvar var in 
      (translated_exp, var_type, Continue)
    | NilExp -> ((), Types.NIL, Continue)
    | IntExp _ -> ((), Types.INT, Continue)
    | StringExp _ -> ((), Types.STRING, Continue)
    | CallExp {func; args; pos} ->
      begin
      match (Symbol.look venv func) with
      | Some real_func -> 
        begin
        match real_func with
        | Env.FunEntry {level=_; formals; result} ->
          let types_of_formals = 
            List.map get_exp_type_no_break args
          in
          if List.equal type_eq formals types_of_formals then
            ((), result, Continue)
          else
            ErrorMsg.error_no_recover pos "Bad type in function call args"
        | Env.VarEntry _ ->
          ErrorMsg.error_no_recover pos "Expected function"
        end
      | None -> ErrorMsg.error_no_recover pos "Undefined function"
      end
    | OpExp {left; oper; right; pos} ->
      (
        let 
          left_type = get_exp_type_no_break left
        in
        let 
          right_type = get_exp_type_no_break right
        in
        match oper with
        | PlusOp | MinusOp | TimesOp | DivideOp ->
          (
            if both_type left_type right_type INT then
              ((), Types.INT, Continue)
            else
              ErrorMsg.error_no_recover pos "Non-integer operands"
          )
          | LtOp | LeOp | GtOp | GeOp ->
          (
            (*
            if ((check_int left) && (check_int right)) ||
              ((get_exp_type left) = Types.STRING && (get_exp_type right) = Types.STRING) then*)
            if both_type left_type right_type INT || 
                both_type left_type right_type STRING then
              ((), Types.INT, Continue)
            else
              ErrorMsg.error_no_recover pos "Operands must either both be integers or strings"
          )
          | EqOp | NeqOp -> 
            (match left_type, right_type with
              | INT, INT| STRING, STRING | ARRAY _, ARRAY _ | RECORD _, RECORD _ 
              | RECORD _, NIL | NIL, RECORD _ ->
                ((), Types.INT, Continue)
              | _ -> ErrorMsg.error_no_recover pos "Invalid operands"
            )
      )
    | RecordExp {fields; typ; pos} ->
      (
        match Symbol.look tenv typ with
        | Some wrapped_real_typ ->
          (
            let 
              real_typ = Types.actual_ty wrapped_real_typ 
            in
            let asymbol_to_record_field (asym, exp, _) = 
              (asym, get_exp_type_no_break exp)
            in
            let 
              record_fields = List.map asymbol_to_record_field fields
            in
            match real_typ with
            | Types.RECORD (formals, _) ->
              let field_sym_eq (symb, ty) (esymb, ety) =
                ((Symbol.name symb) = (Symbol.name esymb)) &&
                  Types.(ty = ety)
              in
              if List.equal field_sym_eq record_fields formals then
                ((), real_typ, Continue)
              else 
                ErrorMsg.error_no_recover pos "Type or name mismatch in record"
            | _ -> ErrorMsg.error_no_recover pos "Type isn't a record"
          )
        | None -> ErrorMsg.error_no_recover pos "Undefined type"
      )
    | SeqExp elst -> 
      let rec eval_seqexp lst =
        match lst with
        | [] -> ((), Types.UNIT, Continue) (*Very, very ugly hack to handle lack of UnitExp*)
        | [(exp, _)] -> trexp exp
        | (exp, _) :: lst -> let _ = (trexp exp : expty) in eval_seqexp lst   
      in
      eval_seqexp elst
    | AssignExp {var; exp; pos} ->
      let (_, var_typ) = trvar var in
      let exp_typ = get_exp_type_no_break exp in
      if Types.(var_typ = exp_typ) then
        ((), Types.UNIT, Continue)
      else
        ErrorMsg.error_no_recover pos "Mismatch between variable and expression type"
    | IfExp {test; then_; else_; pos} ->
      if check_int test then
        (
          let then_typ = get_exp_type_no_break then_ in
          match else_ with
          | Some real_else ->
            let real_else_type = 
              get_exp_type_no_break real_else
            in
            if Types.(then_typ = real_else_type) then
              ((), then_typ, Continue)
            else
              ErrorMsg.error_no_recover pos "then and else body type mismatch"
          | None -> 
            if Types.(then_typ = UNIT) then
              ((), Types.UNIT, Continue)
            else
              ErrorMsg.error_no_recover pos "then statement cant' returna value"
        )
      else
        ErrorMsg.error_no_recover pos "Test isn't an int"
    | WhileExp {test; body; pos} ->
      if check_int test then
        let (_, body_type, _) = trexp body in
        if Types.(body_type = UNIT) then
          ((), Types.UNIT, Continue)
        else
          ErrorMsg.error_no_recover pos "While statements can't return a value"
      else
        ErrorMsg.error_no_recover pos "Test isn't an int"
    | ForExp {var; escape; lo; hi; body; pos} ->
      if check_int lo then
        if check_int hi then
          let for_counter_access = 
            Env.VarEntry{access= Translate.allocLocal level !escape; ty=Types.INT}
          in
          let for_venv = 
            Symbol.enter venv var for_counter_access
          in
          let (_, exptyp, _) = 
          transExp for_venv tenv body level 
          in
            if Types.(exptyp = UNIT) then
              ((), Types.UNIT, Continue)
            else
              ErrorMsg.error_no_recover pos "For statement body must return unit"
        else
          ErrorMsg.error_no_recover pos "Hi of for statement must be int"
      else
        ErrorMsg.error_no_recover pos "lo of for statement must return int"
    | BreakExp _ -> 
        ((), UNIT, Break) 
    | ArrayExp {typ; size; init; pos} ->
      (match Symbol.look tenv typ with
      | Some wrapped_real_typ ->
        let real_typ = 
          Types.actual_ty wrapped_real_typ 
        in
        (match real_typ with
          | Types.ARRAY (arrty, _) ->
            if check_int size then
              let init_type = 
                get_exp_type_no_break init
              in
              if Types.(init_type = arrty) then
                ((), real_typ, Continue)
              else
                ErrorMsg.error_no_recover pos "Init type must match array element type"
            else
              ErrorMsg.error_no_recover pos "Size must be int type"
          | _ -> ErrorMsg.error_no_recover pos "Type must be an array type"
        )
      | None -> ErrorMsg.error_no_recover pos "Undefined type")
    | LetExp{decs; body; _} ->
      let add_dec (venv, tenv) dec = 
        transDec venv tenv dec level
      in
      let (venv_, tenv_) = 
        List.fold_left add_dec (venv, tenv) decs
      in
      transExp venv_ tenv_ body level
  and trvar var =
    match var with
    | SimpleVar (sym, pos) ->
      (match Symbol.look venv sym with
      | Some real_sym ->
        (match real_sym with
        | VarEntry{access=_; ty} -> ((), Types.actual_ty ty)
        | _ -> ErrorMsg.error_no_recover pos "Variable expected but got function")
      | None -> ErrorMsg.error_no_recover pos "Undefined variable")
    | FieldVar (var, sym, pos) ->
      let 
        (_, varty) = trvar var 
      in
      (match varty with
      | Types.RECORD (flst, _) -> 
        let rec get_field_type field_lst =
          (match field_lst with
          | (other, other_ty) :: t -> 
              if (Symbol.name other) = (Symbol.name sym) then 
                ((), Types.actual_ty other_ty) 
              else 
                get_field_type t
          | [] -> ErrorMsg.error_no_recover pos "Field doesn't exist in record"
          )
        in
        get_field_type flst
      | _ -> ErrorMsg.error_no_recover pos "Variable isn't a record")
    | SubscriptVar (var, exp, pos) ->
      let (_, varty) = 
        trvar var 
      in
      (match varty with
      | Types.ARRAY (arrty, _) -> 
        if check_int exp then
          ((), Types.actual_ty arrty)
        else
        ErrorMsg.error_no_recover pos "Array subscript not an integer"
      | _ -> ErrorMsg.error_no_recover pos "Subscript used on non-array type")
  (*By-default assumes that break statements aren't allowed*)
  and get_exp_type_no_break e =
    let (_, ty, is_break) = trexp e in 
    match is_break with
    | Continue -> ty
    | Break -> failwith "Unclosed break"
  and check_int e =
    (get_exp_type_no_break e) = Types.INT
  in
    trexp exp
and trans_exp_no_break (venv : venv) (tenv : tenv) (exp : A.exp) (level : Translate.level) =
  let (translated_exp, exp_type, is_break) = transExp venv tenv exp level in
  match is_break with
  | Continue -> (translated_exp, exp_type)
  | Break -> failwith "Break statement without a loop"
and transDec (venv : venv) (tenv : tenv) (dec: A.dec) (level : Translate.level)=
  let translate_params params = 
      List.map (fun ({name=_; escape=_; typ; pos} : A.field) -> 
        (
          match Symbol.look tenv typ with
          | Some real_typ -> real_typ
          | None -> ErrorMsg.error_no_recover pos "Undefined type"
        )) params
  in
  let add_func_helper
    rvenv expected_typ ({ name; params; result=_; body; pos } : A.fundec) =
    (*TODO: add check that name is in rvenv*)
    let find_func_level func_name = 
      match Symbol.look rvenv func_name with
      | Some val_entry ->
        begin
        match val_entry with
        | Env.FunEntry {result=_; formals=_; level} ->
          level
        | Env.VarEntry _ -> ErrorMsg.error_no_recover pos "Function invoked as variable"
        end
      | None -> ErrorMsg.error_no_recover pos "Function not entered prior to call"
    in
    let func_level = find_func_level name in
    let enter_func_field venv ({name; escape; typ; pos} : A.field) = 
      match Symbol.look tenv typ with
      | Some real_typ -> 
        let arg_access = Translate.allocLocal func_level !escape in
        let arg_var = Env.VarEntry{access=arg_access; ty=real_typ} in
        Symbol.enter venv name arg_var
      | None -> ErrorMsg.error_no_recover pos "Undefined type"
    in
    let func_venv = List.fold_left enter_func_field rvenv params in
    let (_, bodytyp) = trans_exp_no_break func_venv tenv body func_level in
    if Types.(bodytyp = expected_typ) then
      let func_params = translate_params params in
      let func_entry = 
        Env.FunEntry{level=func_level; result=Types.UNIT;formals=func_params} 
      in
      (Symbol.enter rvenv name func_entry)
    else 
      ErrorMsg.error_no_recover pos "Type of a procedure must be UNIT"
  in
  let add_func rvenv (fdec : A.fundec) =
    match fdec.result with
    | Some (expectedtyp, pos) ->( 
      match Symbol.look tenv expectedtyp with
      | Some real_typ -> 
        add_func_helper rvenv real_typ fdec
      | None -> 
        ErrorMsg.error_no_recover pos "Undefined type"
    )
    | None -> add_func_helper rvenv Types.UNIT fdec
  in
  match dec with
  | A.VarDec {name; escape; typ; init; pos} ->
      (match typ with
      | Some (expected_typ, pos) ->
        (
          match Symbol.look tenv expected_typ with 
          | Some expected_typ ->
            let (_, actual_typ) = trans_exp_no_break venv tenv init level in
            if Types.(expected_typ = actual_typ) then
              let var_access = Translate.allocLocal level !escape in
              let var_entry = Env.VarEntry{access=var_access; ty=expected_typ} in
              (Symbol.enter venv name var_entry, tenv)
            else
              ErrorMsg.error_no_recover pos "Mismatched type in variable declaration"
          | None -> ErrorMsg.error_no_recover pos "Undefined type"
        )
      | None -> let (_, actual_typ) = trans_exp_no_break venv tenv init level in 
        if actual_typ = Types.NIL then
          ErrorMsg.error_no_recover pos "Variable initialized with nil must be type marker"
        else
          let var_access = Translate.allocLocal level !escape in
          let var_entry = Env.VarEntry{access=var_access; ty=actual_typ} in
          (Symbol.enter venv name var_entry, tenv)
      )
  | A.TypeDec tlst ->
    let add_type_header tenv_ ({name; ty=_; pos=_} : A.atypedec) = 
      let type_header = Types.NAME (name, ref None) in
      Symbol.enter tenv_ name type_header
    in
    let add_type tenv_ ({name; ty; _} : A.atypedec) =
      match Symbol.look tenv_ name with
      | Some (Types.NAME (_, actual_type)) -> 
        actual_type := (Some (transTy tenv_ ty));
        tenv_
      | Some _ 
      | None ->
        Symbol.enter tenv_ name (transTy tenv_ ty)
    in
    (*returns true if there is a name cycle, false otherwise*)
    let rec check_graph_cycle encountered_names first_chain =
      match first_chain with
      | Types.NAME (other_name, storedty) ->
        if List.mem (Symbol.name other_name) encountered_names then
          true
        else
          (match !storedty with
          | Some realstoredty -> 
              let encountered_names = ((Symbol.name other_name) :: encountered_names) in
              check_graph_cycle encountered_names realstoredty
          | None -> false)
      | _ -> false
    in
    (*returns true if there is a simple cycle, false otherwise*)
    let rec cycle_check tenv_ (added_types : A.atypedec list) = 
      match added_types with
      | [] -> ()
      | ({name=starting_name; ty=ty; pos=pos}) :: added_types ->
        if check_graph_cycle [(Symbol.name starting_name)] (transTy tenv_ ty) then
          ErrorMsg.error_no_recover pos "Simple cycle detected in type declaration!"
        else 
          cycle_check tenv_ added_types
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
    let header_tenv = List.fold_left add_type_header tenv tlst in
    let result_tenv = List.fold_left add_type header_tenv tlst in
    cycle_check result_tenv tlst;
    ( venv, result_tenv)
  | A.FunctionDec func_lst ->
    let enter_func_header_result venv name params result_type  =
      let escape_of_field ({name=_; escape; typ=_; pos=_} : A.field) = !escape in
      let escape_params = List.map escape_of_field params in
      let func_label = Temp.newLabel () in
      let func_level = Translate.newLevel level func_label escape_params in
      let func_entry = 
        Env.FunEntry{level=func_level; 
                    formals = translate_params params;
                    result=result_type}
      in
      Symbol.enter venv name func_entry
    in
    let enterFuncHeader venv ({ name; params; result; body=_; pos=_ } : A.fundec) = 
      match result with
      | Some (result, pos) ->
        (match Symbol.look tenv result with
        | Some result ->
            enter_func_header_result venv name params result
        | None -> ErrorMsg.error_no_recover pos "Undefined type")
      | None ->
        enter_func_header_result venv name params Types.UNIT
    in
    (*throws exception of functions with same name are found, otherwise unit*)
    let rec check_same_name_helper (func_lst : A.fundec list) declared = 
      match func_lst with
      | [] -> ()
      | ({name; params=_; result=_; body=_; pos;} : A.fundec) :: func_lst ->
        if List.mem name declared then
          ErrorMsg.error_no_recover pos "Function has same name as previous one in recursive block"
        else
          check_same_name_helper func_lst (name :: declared)
    in
    let check_same_name func_lst = 
      check_same_name_helper func_lst [] 
    in
    check_same_name func_lst; 
    let recursive_venv = 
      List.fold_left enterFuncHeader venv func_lst 
    in
    (List.fold_left add_func recursive_venv func_lst, tenv)

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
  let (_, _) = trans_exp_no_break Env.base_venv Env.base_tenv exp Translate.outermost in
  ()