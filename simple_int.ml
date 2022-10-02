let multiple_of n d =
  n mod d = 0

let integer_square_root n =
  let rec aux m = 
    if (m * m) <= n then m else aux (m - 1)
  in
  aux (n - 1)
