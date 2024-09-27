type unique = unit ref

type ty = INT 
        | STRING
        | RECORD of (Symbol.t * ty) list * unique
        | ARRAY of ty * unique
        | NIL
        | UNIT
        | NAME of Symbol.t * ty option ref