    type symbol = string * int       (* int用于为每个标识符编号 *)

    exception Symbol

    let nextString = ref 0
(* 哈希表用于存储string * int 对 *)
    let table : (string, int) Hashtbl.t = Hashtbl.create 128

    let symbol name =
      try
        (name, Hashtbl.find table name)
      with Not_found ->
        nextString := !nextString + 1;
        Hashtbl.add table name !nextString;
        (name, !nextString)

    let name (s, n) = s

    module Ord =
      struct
        type t = symbol
        let compare (name1, integer1) (name2, integer2) =
          Pervasives.compare integer1 integer2
      end

(* Map 用于存储symbol * binding 对 *)
    module Table = Map.Make(Ord)
