type stack = int array
exception Full
exception Empty

let create size =
  Array.make (size + 1) 0

let push buf elt =
  let size = buf.(0) in
  if Array.length buf = size + 1
  then raise Full
  else 
    begin
      buf.(0) <- size + 1;
      buf.(size + 1) <- elt;
    end
  
let append buf arr =
  Array.fold_right (fun x _ -> push buf x) arr ()

let pop buf =
  let size = buf.(0) in
  if size = 0
  then raise Empty
  else 
    begin
      buf.(0) <- size - 1;
      buf.(size)
    end
