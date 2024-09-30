type venv = Env.enventry Symbol.table
type tenv = Types.ty Symbol.table
type expty = Translate.exp * Types.ty

module A = Absyn

let get_type (tenv : tenv) (sym : Symbol.t) pos : Types.ty =
  match Symbol.look tenv sym with
  | Some typ -> typ
  | None -> ErrorMsg.error_no_recover pos "Undefined type"
let transExp (venv : venv) (tenv : tenv) (exp : A.exp) : expty =
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
    | _ -> failwith "Not implemented"
  and trvar _ =
    ((), Types.NIL)
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