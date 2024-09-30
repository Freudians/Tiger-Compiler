type unique = unit ref

type ty = INT 
        | STRING
        | RECORD of (Symbol.t * ty) list * unique
        | ARRAY of ty * unique
        | NIL
        | UNIT
        | NAME of Symbol.t * ty option ref

let actual_ty (typ : ty) : ty =
        match typ with
        | NAME (_, wrap_ty) ->
                begin
                match !wrap_ty with
                | Some real_ty -> real_ty
                | None -> typ
                end
        | _ -> typ
let ( = ) (t1 : ty) (t2 : ty) = 
        match (actual_ty t1), (actual_ty t2) with
        | RECORD (_, uqid1), RECORD (_, uqid2) ->
                uqid1 == uqid2
        | ARRAY (_, uqid1), ARRAY (_, uqid2) ->
                uqid1 == uqid2
        | RECORD _, NIL -> true
        | NIL, RECORD _ -> true
        | NAME (id1, _), NAME (id2, _) ->
                if (Symbol.name id1) = (Symbol.name id2) then
                        true
                else
                        false
        | t1_real, t2_real -> Stdlib.( = ) t1_real t2_real
