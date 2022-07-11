let empty : heap =
  E

let rank (h : heap) : rank =
  match h with
    E -> 0
  | T (r, _, _, _) -> r

let makeT (x : element) (h1 : heap) (h2 : heap) : heap =
  let r1, r2 = rank h1, rank h2 in
  let rank, left, right = if r1 >= r2 then r2, h1, h2 else r1, h2, h1 in
  let rank = 1 + rank in
  T (rank, x, left, right)

let singleton (x : element) : heap =
  makeT x E E

let rec union (h1 : heap) (h2 : heap) : heap =
  match h1, h2 with
    E, E -> E
  | E, _ -> h2
  | _, E -> h1
  | T (r1, e1, hl1, hr1), T (r2, e2, hl2, hr2) ->
      if priority e1 < priority e2 
      then makeT e1 (union hl1 hr1) h2
      else makeT e2 h1 (union hl2 hr2)

let insert (x : element) (h : heap) : heap =
  union (singleton x) h

let extract (h : heap) : (element * heap) option =
  match h with
    E -> None
  | T (_, e, h1, h2) -> Some (e, union h1 h2)
