type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

exception Unbalanced of int ;;

let rec height = function
  | Empty -> 0, 1
  | Node (t, _, t') -> 
      let lh, l = height t in
      let rh, r = height t' in
      1 + (max lh rh), l + r

let rec balanced = function
  | Empty -> true, 1
  | Node (t, _, t') ->
      let lb, l = balanced t in
      if lb 
      then 
        let rb, r = balanced t' in
        if lb && rb
        then 
          let lh, x = height t in
          let rh, y = height t' in
          lh = rh, l + r + x + y
        else
          false, l + r 
      else false, l 

let bal_height bst =
  let rec aux t n = 
    match t with
    | Empty -> 0, 1 + n
    | Node (t, _, t') ->
        let lh, l = aux t n in
        let rh, r = aux t' l in
        if lh <> rh
        then raise (Unbalanced (r - 1))
        else 1 + lh, r
  in  
  aux bst 0
    
let balanced_fast bst =
  try
    let _, n = bal_height bst in
    (true, n)
  with
  | Unbalanced n -> (false, n) 