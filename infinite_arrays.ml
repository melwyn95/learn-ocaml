
type 'a t = ('a array * 'a * int) ref

let make (x : 'a) : 'a t =
  ref (Array.make 1 x, x, 1)

let get (a : 'a t) (i : int) : 'a =
  let xs, x, length = !a in
  if i >= length then
    let diff = i - length in
    let new_length = (if (diff + length) >= 2 * length 
                      then (diff + length)
                      else 2 * length) + 1 in
    let ys = Array.make new_length x in
    let () = Array.blit xs 0 ys 0 length in
    let () = a := (ys, x, new_length) in
    ys.(i)
  else
    xs.(i)

let set (a : 'a t) (i : int) (y : 'a) : unit =
  let xs, x, length = !a in
  if i >= length then
    let diff = i - length in
    let new_length = (if (diff + length) >= 2 * length 
                      then (diff + length)
                      else 2 * length) + 1 in
    let ys = Array.make new_length x in
    let () = Array.blit xs 0 ys 0 length in
    let () = a := (ys, x, new_length) in
    ys.(i) <- y
  else
    xs.(i) <- y