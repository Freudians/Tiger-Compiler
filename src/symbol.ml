  type t = string * int
  module H = Hashtbl

  (*exception Symbol*)
  let nextsym = ref 0
  let size_hint = 128
  let hashtable = H.create size_hint
  let symbol name = 
    let count = 
    try H.find hashtable name with
    | Not_found -> 
      let i = !nextsym in
        nextsym := i + 1;
        H.add hashtable name i; 
        i
    in
      (name, count)
  let name (n, _) = n

  let get_int ((_, i) : t) = i
  
  module BTable = Map.Make(
    struct
    type t = string * int
    let compare s1 s2 = get_int(s1) - get_int(s2)
    end 
  )

  type 'a table = 'a BTable.t

  let empty = BTable.empty
  let enter tab s ty = BTable.add s ty tab
  let look tab s = 
    try Some (BTable.find s tab) with
    | Not_found -> None
