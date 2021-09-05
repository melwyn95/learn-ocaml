let rec even xs =
  match xs with
  | [] -> []
  | x::[] -> x::[]
  | x::xs' -> x::(odd xs')
  
and odd xs =
  match xs with
  | [] -> []
  | _::[] -> []
  | _::y::[] -> y::[]
  | _::y::_::xs' -> y::(even xs')
  
let rec merge xs ys =
  match (xs,ys) with
  | [], ys' -> ys'
  | xs', [] ->  xs'
  | x::xs', y::ys' -> 
      if x < y 
      then x::(merge xs' (y::ys')) 
      else y::(merge (x::xs') ys')
  
let rec sort xs =
  match xs with
  | [] -> [] 
  | x::[] -> x::[]
  | _ ->
      let ev = even xs in
      let od = odd xs in
      merge (sort ev) (sort od)
        
let rec uniq xs =
  match xs with
  | [] -> []
  | x::[] -> x::[]
  | x::y::xs' -> if x = y then uniq (y::xs') else x::(uniq (y::xs'))
                                                     
let weed xs = uniq @@ sort xs
