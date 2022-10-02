module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
end

module CharHashedType = struct 
  type t = char
  let equal c c' = c = c'
  let hash = int_of_char 
end

module CharHashtbl = Hashtbl.Make(CharHashedType)

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let empty () = Trie (None, CharHashtbl.create(1024))

  let lookup trie w =
    let rec aux i n trie =
      if i < n then
        match trie with
          Trie (io, tbl) ->
            let c = w.[i] in
            let t = CharHashtbl.find_opt tbl c in
            match t with
              None -> None
            | Some t -> aux (i + 1) n t
      else match trie with
          Trie (io,_) -> io
    in
    aux 0 (String.length w) trie

  let insert trie w v =
    let rec aux i n t =
      if i < n then 
        match t with
          Trie (_, tbl) ->
            let c = w.[i] in
            let tt = CharHashtbl.find_opt tbl c in
            match tt with
              None -> 
                CharHashtbl.add tbl c (aux (i + 1) n (empty ()));
                t
            | Some ttt -> 
                let _ = aux (i + 1) n ttt in
                t
      else
        Trie (Some v, CharHashtbl.create(1024))
    in
    aux 0 (String.length w) trie
end