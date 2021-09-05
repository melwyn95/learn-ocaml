let make () : elem = ref @@ Root 0

let find (x : elem) : elem =
  let rec aux (x : elem) (xs : elem list) =
    match !x with
    | Root _ -> 
        let () = x := Root (List.length xs) in
        let () = List.iter (fun x' -> x' := Link x) xs in
        x
    | Link x' -> 
        aux x' (x::xs) 
  in
  aux x []
  
let eq (x : elem) (y : elem) : bool = (find x) == (find y)

let link (x : elem) (y : elem) : unit =
  match (!x, !y) with
  | Root x_r, Root y_r ->
      if y_r < x_r then y := Link x else x := Link y
  | _ -> raise TODO

let union (x : elem) (y : elem) : unit = 
  let x = find x in
  let y = find y in
  if x == y
  then ()
  else link x y
  
let rec print_elem e = 
  match !e with
  | Root _ -> print_endline "Root"
  | Link x -> let () = print_endline "Link" in
      print_elem x
