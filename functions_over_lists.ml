let rec mem x l =
  match l with
    [] -> false
  | y::xs -> x = y || mem x xs

let rec append l1 l2 =
  match l1, l2 with
    [], l2 -> l2 
  | x::xs, l2 ->
      let l2 = append xs l2 in
      x :: l2

let rec combine l1 l2 =
  match l1, l2 with
    [], [] -> []
  | x::xs, y::ys -> (x,y) :: combine xs ys
  | _ -> failwith "List are not of equal length"
                      
let rec assoc l k =
  match l with
    [] -> None
  | (k',v)::xs -> 
      if k = k' 
      then Some v
      else assoc xs k