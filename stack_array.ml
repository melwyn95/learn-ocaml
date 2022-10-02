type stack = int array
exception Full
exception Empty

let create size =
  Array.make (size + 1) 0

let push buf elt =
  if buf.(0) = (Array.length buf - 1) then raise Full
  else
    let () = buf.(0) <- buf.(0) + 1 in
    buf.(buf.(0)) <- elt 

let append buf arr =
  Array.fold_right (fun x () -> push buf x) arr ()

let pop buf =
  if buf.(0) = 0 then raise Empty
  else
    let x = buf.(buf.(0)) in
    buf.(0) <- buf.(0) - 1;
    x
