(* A type of binary trees. *)

type 'a tree =
  | Leaf
  | Node of 'a tree * 'a * 'a tree

(* The module [Seq] is standard as of OCaml 4.07. *)

module Seq = struct

  type 'a t = unit -> 'a node

  and +'a node =
  | Nil
  | Cons of 'a * 'a t

  let nil () = Nil

  let cons x xs () = Cons (x, xs)

  (* The sequence [trap] always raises the exception [Trap] when
     it is queried. It is used by the automatic grader and can
     appear in some messages produced by the grader. *)

  exception Trap

  let trap () = raise Trap

end

(* Producing lists. *)

let rec slow_elements (t : 'a tree) : 'a list =
  match t with
  | Leaf -> []
  | Node (l,v,r) -> (slow_elements l) @ [v] @ slow_elements r
;;
let rec elements_with (t : 'a tree) (ys : 'a list) : 'a list =
  match t with
  | Leaf -> ys
  | Node (l,v,r) -> 
      let ys = elements_with r ys in
      let ys = elements_with l (v :: ys) in
      ys
;;

let elements (t : 'a tree) : 'a list =
  elements_with t []

(* Producing on-demand sequences. *)

let rec fringe_seq_with (t : 'a tree) (ys : 'a Seq.t) : 'a Seq.t =
  match t with
  | Leaf -> ys
  | Node (l,v,r) ->
      let ys = fun () -> fringe_node_with t ys in
      let ys = fringe_seq_with l ys in
      ys

and fringe_node_with (t : 'a tree) (ys : 'a Seq.t) : 'a Seq.node =
  match t with
  | Leaf -> Seq.Nil
  | Node (l,v,r) -> Seq.Cons (v, fringe_seq_with r ys)
;;

let fringe (t : 'a tree) : 'a Seq.t =
  fringe_seq_with t Seq.nil

(* Comparing sequences. *)

let rec equal (xs : 'a Seq.t) (ys : 'a Seq.t) : bool =
  match xs (), ys () with
  | Seq.Nil, Seq.Nil -> true
  | Seq.Cons (x, xs), Seq.Cons (y, ys) -> x = y && equal xs ys
  | _ -> false
  

(* Comparing trees. *)

let same_fringe (t1 : 'a tree) (t2 : 'a tree) : bool =
  let xs = fringe t1 in
  let ys = fringe t2 in
  equal xs ys
