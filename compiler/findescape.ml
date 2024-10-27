(*General algo: 
Since pass-by-reference and dereferencing is illegal in Tiger,
the only case where we know a variable escapes ahead of time is if
it is referenced in a nested function
therefore, we only have to check if each variable is used in a function of lower depth*)
module A = Absyn
type depth = int 
type escape_env = (depth * bool ref) Symbol.table

let rec traverse_exp (env : escape_env) (d : depth) (exp : A.exp) = 
    match exp with
    | A.VarExp var -> traverse_var env d var
    | NilExp | IntExp _ | StringExp _ -> ()
    | CallExp {func=_; args; pos=_} ->
        List.fold_left (fun _ arg -> traverse_exp env d arg) () args
    | OpExp {left; oper=_; right; pos=_} ->
        traverse_exp env d left;
        traverse_exp env d right
    | RecordExp {fields; typ=_; pos=_} ->
        let traverse_field_exp (_, exp, _) = traverse_exp env d exp
        in
        List.fold_left (fun _ field -> traverse_field_exp field) () fields
    | SeqExp explist ->
        List.fold_left (fun _ (exp, _) -> traverse_exp env d exp) () explist
    | AssignExp {var; exp; pos=_} ->
        traverse_exp env d exp;
        traverse_var env d var
    | IfExp {test; then_; else_; pos=_} ->
        traverse_exp env d test; 
        traverse_exp env d then_;
        (match else_ with 
        | Some else_ -> traverse_exp env d else_ 
        | None -> ())
    | WhileExp {test; body; pos=_} ->
        traverse_exp env d test;
        traverse_exp env d body
    | ForExp {var; escape; lo; hi; body; pos=_} ->
        let for_env = Symbol.enter env var (d, escape) in
        escape := false;
        traverse_exp for_env d lo;
        traverse_exp for_env d hi;
        traverse_exp for_env d body
    | BreakExp _ -> ()
    | LetExp {decs; body; pos=_} ->
        let body_env = 
            List.fold_left (fun env dec -> traverse_dec env d dec) env decs
        in
        traverse_exp body_env d body
    | ArrayExp {typ=_; size; init; pos=_} -> 
        traverse_exp env d size;
        traverse_exp env d init
and traverse_var (env : escape_env) (d : depth) (var : A.var) =
    match var with 
    | SimpleVar (name, pos) ->
        (match Symbol.look env name with
        | Some (var_depth, isEscape) ->
            if d > var_depth then isEscape := true
        | None -> ErrorMsg.error_no_recover pos "Undefined variable used"
        )
    | FieldVar (real_var, _, _) ->
        traverse_var env d real_var
    | SubscriptVar (var, idx_exp, _) ->
        traverse_exp env d idx_exp;
        traverse_var env d var
and traverse_dec (env : escape_env) (d : depth) (dec : A.dec) =
    match dec with 
    | TypeDec _ -> env 
    | VarDec {name; escape; typ=_; init; pos=_} ->
        traverse_exp env d init;
        escape := false;
        Symbol.enter env name (d, escape)
    | FunctionDec func_list ->
        let func_depth = d+1 in
        let traverse_field env ({name; escape; typ=_; pos=_} : A.field) =
            escape := false;
            Symbol.enter env name (func_depth, escape)
        in
        let traverse_func_dec env
            ({name=_; params; result=_; body; pos=_} : A.fundec) =
            let func_env = 
                List.fold_left (fun env f -> traverse_field env f) env params
            in
            traverse_exp func_env func_depth body;
            env
        in
        List.fold_left traverse_func_dec env func_list
let find_escape (prog : A.exp) = 
    traverse_exp Symbol.empty 0 prog 