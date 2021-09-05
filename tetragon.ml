let pairwise_distinct (lup, rup, llp, rlp) =
  lup <> rup && lup <> llp && lup <> rlp &&
  rup <> llp && rup <> rlp &&
  llp <> rlp

let wellformed ((lup_x, lup_y), (rup_x, rup_y), (llp_x, llp_y), (rlp_x, rlp_y)) =
  lup_x < rup_x && lup_x < rlp_x &&
  llp_x < rup_x && llp_x < rlp_x &&
  lup_y > llp_y && rup_y > rlp_y

let rotate_point (x, y) = (y, -x)
  
let reorder (p1, p2, p3, p4) =
  let [l1;l2;r1;r2] = List.sort (fun (x1,_) (x2,_) -> compare x1 x2) [p1;p2;p3;p4] in 
  let (l1x,l1y) = l1 in
  let (l2x,l2y) = l2 in
  let (r1x,r1y) = r1 in
  let (r2x,r2y) = r2 in
  let (l1x,l1y),(l2x,l2y) = if l1y > l2y then (l1x,l1y),(l2x,l2y) else (l2x,l2y),(l1x,l1y) in
  let (r1x,r1y),(r2x,r2y) = if r1y > r2y then (r1x,r1y),(r2x,r2y) else (r2x,r2y),(r1x,r1y) in
  ((l1x,l1y), (r1x,r1y), (l2x,l2y), (r2x,r2y))
  
let rotate_tetragon (lup, rup, llp, rlp) =
  reorder (rotate_point rlp, rotate_point lup, rotate_point rup, rotate_point llp)

