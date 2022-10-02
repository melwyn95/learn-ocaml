type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))

let rec to_list l =
  match l with
    CSingle x -> [x]
  | CEmpty -> []
  | CApp (x, xs) -> (to_list x) @ to_list xs

let rec of_list l =
  match l with
    [] -> CEmpty
  | x::xs -> CApp (CSingle x, of_list xs)

let append l1 l2 =
  match l1, l2 with
    CEmpty, l2 -> l2
  | l1, l2 -> CApp (l1, l2)

let rec hd : 'a clist -> 'a option = fun l ->
  match l with
    CEmpty -> None
  | CSingle x -> Some x
  | CApp (l, r) ->
      (match hd l with
         Some x -> Some x
       | None -> hd r)

let rec tl : 'a clist -> 'a clist option = fun l ->
  match l with
    CEmpty -> None
  | CSingle _ -> Some CEmpty
  | CApp (l, r) -> 
      (match tl l with
         Some l -> Some (CApp (l, r))
       | None -> tl r)
