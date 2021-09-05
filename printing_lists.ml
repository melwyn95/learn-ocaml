let rec print_int_list l =
  match l with
  | [] -> ()
  | x::xs -> 
      let () = print_int x in
      let () = print_newline () in
      print_int_list xs

let print_every_other k l =
  let rec aux i xs = 
    match xs with
    | [] -> []
    | x::xs -> if i mod k = 0 then x :: aux (i+1) xs else aux (i+1) xs 
  in
  print_int_list @@ aux 0 l

let rec print_list print l =
  match l with
  | [] -> ()
  | x::xs -> 
      let () = print x in
      let () = print_newline () in
      print_list print xs
