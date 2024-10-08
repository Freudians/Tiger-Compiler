module PrintAbsyn : 
  sig val print : out_channel * Absyn.exp -> unit end =
struct

  module A = Absyn

let print (outstream, e0) =
  let say s = output_string outstream s in
  let sayln s = (say s; say "\n") in
  let say_pos (pos : Lexing.position) = Printf.printf "%d" pos.pos_lnum in

  let rec indent = function
    | 0 -> ()
    | i -> (say " "; indent (i-1))
  in

  let opname = function
    | A.PlusOp -> "PlusOp"
    | A.MinusOp -> "MinusOp"
    | A.TimesOp -> "TimesOp"
    | A.DivideOp -> "DivideOp"
    | A.EqOp -> "EqOp"
    | A.NeqOp -> "NeqOp"
    | A.LtOp -> "LtOp"
    | A.LeOp -> "LeOp"
    | A.GtOp -> "GtOp"
    | A.GeOp -> "GeOp"
  in

  let rec dolist d f = function
    | [a] -> (sayln ""; f (a, d+1))
    | a::r -> (sayln ""; f (a, d+1); say ","; dolist d f r)
    | [] -> ()
  in

  let rec var (v, d) = match v with
    | A.SimpleVar(s, pos) -> 
        (indent d; say "SimpleVar("; say (Symbol.name s); 
        sayln ","; say_pos pos; say ")")
    | A.FieldVar(v, s, pos) -> 
        (indent d; sayln "FieldVar(";
         var (v, d+1); sayln ",";
         indent (d+1); say (Symbol.name s); 
         sayln ","; say_pos pos;
         say ")")
    | A.SubscriptVar(v, e, pos) -> 
        (indent d; sayln "SubscriptVar(";
         var (v, d+1); sayln ",";
         exp (e, d+1); 
         sayln ","; say_pos pos;
         say ")")

  and exp (e, d) = match e with
    | A.VarExp v -> (indent d; sayln "VarExp("; var (v, d+1); say ")")
    | A.NilExp -> (indent d; say "NilExp")
    | A.IntExp i -> (indent d; say "IntExp("; say (string_of_int i); say ")")
    | A.StringExp(s, pos) -> (indent d; say "StringExp(\""; say s; say "\""; 
    sayln ","; say_pos pos;
    say ")")
    | A.CallExp{func; args; pos} ->
        (indent d; say "CallExp("; say (Symbol.name func);
         say ",["; dolist d exp args; 
         say "]";  sayln ","; say_pos pos;
         say ")")
    | A.OpExp{left; oper; right; _} ->
        (indent d; say "OpExp("; say (opname oper); sayln ",";
         exp (left, d+1); sayln ","; exp (right, d+1); say ")")
    | A.RecordExp{fields; typ; _} ->
        let f ((name, e, _), d) = 
          (indent d; say "("; say (Symbol.name name);
           sayln ","; exp (e, d+1);
           say ")")
        in
        (indent d; say "RecordExp("; say (Symbol.name typ); 
         sayln ",["; dolist d f fields; say "])")
    | A.SeqExp l -> 
        (indent d; say "SeqExp["; dolist d exp (List.map fst l); say "]")
    | A.AssignExp{var=v; exp=e; _} -> 
        (indent d; sayln "AssignExp("; var (v, d+1); sayln ",";
         exp (e, d+1); say ")")
    | A.IfExp{test; then_; else_; _} ->
        (indent d; sayln "IfExp("; exp (test, d+1); sayln ",";
         exp (then_, d+1);
         (match else_ with
          | None -> ()
          | Some e -> (sayln ","; exp (e, d+1)));
         say ")")
    | A.WhileExp{test; body; _} ->
        (indent d; sayln "WhileExp("; exp (test, d+1); sayln ",";
         exp (body, d+1); say ")")
    | A.ForExp{var=v; escape=b; lo; hi; body; _} ->
        (indent d; sayln "ForExp(";
         say (Symbol.name v); say ","; say (string_of_bool !b); sayln ",";
         exp (lo, d+1); sayln ","; exp (hi, d+1); sayln ",";
         exp (body, d+1); say ")")
    | A.BreakExp _ -> (indent d; say "BreakExp")
    | A.LetExp{decs; body; _} ->
        (indent d; say "LetExp([";
         dolist d dec decs; sayln "],"; exp (body, d+1); say")")
    | A.ArrayExp{typ; size; init; _} ->
        (indent d; say "ArrayExp("; say (Symbol.name typ); sayln ",";
         exp (size, d+1); sayln ","; exp (init, d+1); say ")")

  and dec (d', d) = match d' with
    | A.FunctionDec l -> 
      begin
        let field (({name; escape; typ; _} : A.field), d) = 
          (indent d; say "("; say (Symbol.name name);
           say ","; say (string_of_bool !escape); 
           say ","; say (Symbol.name typ); say ")")
        in
        let f (({name; params; result; body; _} : A.fundec), d) =
          (indent d; say "("; say (Symbol.name name); say ",[";
           dolist d field params; sayln "],";
           (match result with
            | None -> say "None"
            | Some(s, _) -> (say "Some("; say (Symbol.name s); say ")"));
           sayln ","; exp (body, d+1); say ")")
        in
        (indent d; say "FunctionDec["; dolist d f l; say "]")
      end
    | A.VarDec{name; escape; typ; init; _} ->
        (indent d; say "VarDec("; say (Symbol.name name); say ",";
         say (string_of_bool !escape); say ",";
         (match typ with
          | None -> say "None" 
          | Some(s, _) -> (say "Some("; say (Symbol.name s); say ")"));
         sayln ","; exp (init, d+1); say ")")
    | A.TypeDec l -> 
        let tdec (({name; ty=t; _} : A.atypedec), d) = 
          (indent d; say"("; 
           say (Symbol.name name); sayln ",";
           ty (t, d+1); say ")")
        in
        (indent d; say "TypeDec["; dolist d tdec l; say "]")
   
  and ty (t, d) = match t with
    | A.NameTy(s, _) -> 
        (indent d; say "NameTy("; say (Symbol.name s); say ")")
    | A.RecordTy l ->  
        let f (({name; escape; typ; _} : A.field), d) =
          (indent d; say "("; say (Symbol.name name);
           say ","; say (string_of_bool !escape); say ",";
           say (Symbol.name typ); say ")")
        in
        (indent d; say "RecordTy["; dolist d f l; say "]")
    | A.ArrayTy(s, _) -> 
        (indent d; say "ArrayTy("; say (Symbol.name s); say ")")

  in 
  exp (e0, 0); 
  sayln ""; 
  flush outstream

end