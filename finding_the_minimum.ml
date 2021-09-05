let min a =
  let rec aux i m n = 
    if i = n 
    then m 
    else aux (i+1) (if a.(i) < m then a.(i) else m) n
  in aux 0 a.(0) (Array.length a)

let min_index a = 
  let rec aux i m mi n = 
    if i = n
    then mi
    else aux 
        (i+1) 
        (if a.(i) < m then a.(i) else m)
        (if a.(i) < m then i else mi) 
        n
  in aux 0 a.(0) 0 (Array.length a)

let it_scales = "no"
  
