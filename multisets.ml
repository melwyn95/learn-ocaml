module type MultiSet_S = sig

  (* A multi-set of type ['a t] is a collection of values of
     type ['a] that may occur several times. *)
  type 'a t

  (* [occurrences s x] return the number of time [x] occurs
     in [s]. *)
  val occurrences : 'a t -> 'a -> int

  (* The empty set has no element. There is only one unique
     representation of the empty set. *)
  val empty : 'a t

  (* [insert s x] returns a new multi-set that contains all
     elements of [s] and a new occurrence of [x]. Typically,
     [occurrences s x = occurrences (insert s x) x + 1]. *)
  val insert : 'a t -> 'a -> 'a t

  (* [remove s x] returns a new multi-set that contains all elements
     of [s] minus an occurrence of [x] (if [x] actually occurs in
     [s]). Typically, [occurrences s x = occurrences (remove s x) x -
     1] if [occurrences s x > 0]. *)
  val remove : 'a t -> 'a -> 'a t

end

module MultiSet = struct
  type 'a t = 'a list
      
  let empty = []
              
  let rec occurrences xs x =
    match xs with
      [] -> 0
    | x'::xs when x = x' -> 1 + occurrences xs x
    | _::xs -> occurrences xs x
                 
  let insert xs x = x :: xs
                    
  let rec remove xs x =
    match xs with
      [] -> []
    | x'::xs when x' = x -> xs
    | x'::xs -> x' :: remove xs x
end ;;

let letters word =
  let rec aux i n m =
    if i < n then 
      aux (i + 1) n (MultiSet.insert m word.[i])
    else m
  in
  aux 0 (String.length word) MultiSet.empty

let anagram word1 word2 =
  let m1 = letters word1 in
  let m2 = letters word2 in
  let rec aux i n =
    if i = n then true else
      let c = word1.[i] in
      let o1 = MultiSet.occurrences m1 c in
      let o2 = MultiSet.occurrences m2 c in
      if o1 = o2 then aux (i + 1) n else false 
  in
  aux 0 (String.length word1)