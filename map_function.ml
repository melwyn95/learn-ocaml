type 'a tree =
    Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;

let wrap l =
  List.map (fun x -> [x]) l

let rec tree_map f = 
  function 
    Leaf a -> Leaf (f a)
  | Node (l, a, r) -> Node ((tree_map f l), (f a), (tree_map f r))
