type queue = int list * int list

let is_empty (front, back) =
  match front, back with [], [] -> true | _ -> false

let enqueue x (front, back) =
  (front, x :: back)
  
let split l =
  let n = List.length l in
  let rec aux i ys xs =
    if i < (n / 2) then
      match xs with
        [] -> failwith "not possible"
      | x::xs -> aux (i + 1) (x::ys) xs
    else (xs, ys)
  in
  let xs, ys = aux 0 [] l in
  let xs = List.rev xs in
  let ys = List.rev ys in
  xs, ys

let dequeue (front, back) =
  match front with
    x::xs -> x, (xs, back)
  | [] -> 
      (match back with
         [] -> failwith "queue is empty"
       | y::ys ->
           let back = List.rev back in
           let e = List.hd back in
           let back = List.tl back in
           let back = List.rev back in
           e, ([], back))