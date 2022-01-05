type tree =
  | Leaf
  | Node of tree * tree

type labeled_tree =
  | LLeaf of int
  | LNode of labeled_tree * labeled_tree

(* Basic constructor functions. *)

let empty = fun _ -> Seq.empty

let just (x : 'a) : 'a enum =
  fun n -> if n = 0 then Seq.singleton x else Seq.empty

let pay (enum : 'a enum) : 'a enum =
  fun n -> if (n - 1) >= 0 then enum (n - 1) else Seq.empty

let sum (enum1 : 'a enum) (enum2 : 'a enum) : 'a enum =
  fun n -> Seq.sum (enum1 n) (enum2 n)

let ( ++ ) =
  sum

let product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
  let rec up i j = if i = j then [j] else i :: up (i + 1) j in
  fun n -> 
    let sizes = up 0 n in
    let enums = List.map (fun s ->
        let s1 = s in
        let s2 = n - s in 
        Seq.product (enum1 s1) (enum2 s2)) sizes in 
    let enum = Seq.bigsum enums in
    enum
      
let ( ** ) =
  product

let map (phi : 'a -> 'b) (enum : 'a enum) : 'b enum =
  fun n -> Seq.map phi (enum n)
  
(* Derived constructor functions. *)

let bit = fun n -> 
  if n = 0 
  then Seq.sum (Seq.singleton 1) (Seq.singleton 0) 
  else Seq.empty

let list (elem : 'a enum) : 'a list enum =
  fun n ->
    let elem_list = Seq.map (fun x -> [x]) (elem 0) in
    let e = fix (fun f -> 
        pay (fun n ->
            if n = 0 then Seq.singleton []
            else
              Seq.map 
                (fun (xs, ys) -> xs @ ys) 
                (Seq.product (f n) elem_list)
          )) in
    e (n + 1)

let tree : tree enum = 
  fun n ->
    fix (
      fun f ->
        pay (fun n ->
            if n = 0 then Seq.singleton Leaf
            else 
              let p = product f f in
              let m = map (fun (l,r) -> Node (l,r)) p in
              m n
          )
    ) (2 * n + 1)

let balanced_product (enum1 : 'a enum) (enum2 : 'b enum) : ('a * 'b) enum =
  (* TO DO: Complete this definition. *)
  raise TODO

let ( *-* ) =
  balanced_product

(* TO DO: Define [balanced_tree]. *)

(* TO DO: Define [balanced_tidy_tree]. *)
