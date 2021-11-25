type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

let hd = List.hd
let tl = List.tl

let rec inorder t =
  match t with
    Leaf -> []
  | Node (u,x,v) -> (inorder u) @ [x] @ (inorder v)

let rec preorder t =
  match t with
    Leaf -> []
  | Node (u,x,v) -> [x] @ (preorder u) @ (preorder v)

let i1, i2 =
  (Node (
      Node(Node(Leaf,'4',Leaf),'2',Node(Leaf,'5',Leaf)),
      '1',
      Node(Node(Leaf,'6',Leaf),'3',Node(Leaf,'7',Leaf))
    )
  ),
  (Node(
      Node(Node(Leaf,'4',Leaf),'2',Node(Leaf,'5',Leaf)),
      '1',
      Node(Leaf,'6',Node(Node(Leaf,'3',Leaf),'7',Leaf))
    )
  )

let p1, p2 =
  (Node (
      Node(Node(Leaf,'4',Leaf),'2',Node(Leaf,'5',Leaf)),
      '1',
      Node(Node(Leaf,'6',Leaf),'3',Node(Leaf,'7',Leaf))
    )
  ),
  (Node (
      Node(Node(Leaf,'4',Leaf),'2',Node(Leaf,'5',Leaf)),
      '1',
      Node(Node(Node(Leaf,'7',Leaf),'6',Leaf),'3',Leaf))
  )
  
let reconstruct inorder preorder =
  let rec partition xs ys x =
    match xs with
      [] -> ([], [])
    | x'::xs -> 
        if x = x' 
        then (List.rev ys,xs) 
        else partition xs (x'::ys) x 
  in
  let rec aux inorder preorder =
    if inorder = [] then 
      preorder,Leaf 
    else if List.length inorder = 1 then 
      (tl preorder),Node (Leaf,hd inorder,Leaf) 
    else match preorder with
        [] -> failwith "inorder empty"
      | head::preorder -> 
          let left,right = partition inorder [] head in
          let preorder,left = aux left preorder in
          let preorder,right = aux right preorder in
          (preorder,Node (left,head,right))
  in
  let _,t = aux inorder preorder in
  t
