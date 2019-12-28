    type symbol = string * int       (* int用于为每个标识符编号 *)

    exception Symbol

    let nextString = ref 0
(* 哈希表用于存储string * int 对 *)
    let hash_table : (string, int) Hashtbl.t = Hashtbl.create 128

    let symbol name =
      try
        (name, Hashtbl.find hash_table name)
      with Not_found ->
        nextString := !nextString + 1;
        Hashtbl.add hash_table name !nextString;
        (name, !nextString)

    let name (s, n) = s

(* Map 用于存储symbol * binding 对 *)
    module Table = Map.Make(struct
        type t = symbol
        let compare (name1, integer1) (name2, integer2) =
          Pervasives.compare integer1 integer2
      end)

(* 多态类型的表 *)
    type 'a table = 'a Table.t

    let empty = Table.empty

(* 添加新的(symbol, binding)对 *)
    let enter table (str, index) binding = Table.add (str, index) binding table

(* 查找绑定 *)
    let look table (str, index) =
      try
        Some(Table.find (str, index) table)
      with Not_found ->
        None
