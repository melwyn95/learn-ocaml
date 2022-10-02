type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
	 ('A', Trie (Some 15, []))])

let rec children_from_char m c =
  match m with
    [] -> None
  | (c',t)::_ when c = c' -> Some t
  | _::m -> children_from_char m c

let rec update_children m c t =
  match m with
    [] -> [c, t]
  | (c',t')::m when c = c' -> (c, t)::m
  | ct::m -> ct::(update_children m c t)

let lookup trie w =
  let rec aux t i n = 
    if i < n then
      let c = w.[i] in
      match t with
        Trie (io, c2c) -> 
          let t = children_from_char c2c c in
          (match t with
             Some t -> aux t (i + 1) n
           | None -> None)
    else
      match t with
        Trie (io,_) -> io
  in
  aux trie 0 (String.length w)

let insert trie w v =
  let e = Trie (None, []) in
  let rec aux t i n =
    if i < n then
      let c = w.[i] in
      match t with
        Trie (io, c2c) ->
          let t' = children_from_char c2c c in
          (match t' with
             Some t' ->
               let c2c = update_children c2c c (aux t' (i + 1) n) in
               Trie (io, c2c)
           | None -> 
               Trie (io, (c, aux e (i + 1) n)::c2c))
    else Trie (Some v, [])
  in
  aux trie 0 (String.length w)
