(* The algebraic data type of random access lists. *)

type 'a seq =
| Nil
| Zero of     ('a * 'a) seq
| One of 'a * ('a * 'a) seq

(* An algebraic data type of arithmetic expressions. *)

type constant =
  int

type var =
  int (* a de Bruijn index *)

type op =
  int -> int -> int

type expr =
  | EConstant of constant
  | EBinOp of expr * op * expr
  | EVar of var
  | ELet of expr * expr

type env =
  constant seq

  let empty = Nil

  let test24 = Zero (One ((2, 4), Nil))
      
  let digits = Zero (One ((0, 1), Zero (One ((((2, 3), (4, 5)), ((6, 7), (8, 9))), Nil))))
  
  (* Measuring the length of a sequence. *)
  
  let rec length : 'a . 'a seq -> int =
    fun xs ->
      match xs with
        Nil -> 0
      | Zero xs -> 2 * length xs
      | One (_, xs) -> 1 + 2 * length xs
  
  let rec cons : 'a . 'a -> 'a seq -> 'a seq =
    fun x xs ->
      match xs with
        Nil -> One (x, Nil)
      | Zero ys -> One (x, ys)
      | One (y, ys) -> Zero (cons (x,y) ys)
    
  let rec uncons : 'a . 'a seq -> ('a * 'a seq) option =
    fun xs ->
      match xs with
        Nil -> None
      | Zero ys -> 
          (match uncons ys with
             None -> None
           | Some ((y1, y2), ys) -> Some (y1, One (y2, ys))) 
      | One (y, Nil) -> Some (y, Nil)
      | One (y, ys) -> Some (y, Zero ys)
  
  let get : 'a . int -> 'a seq -> 'a =
    fun i xs -> 
      let rec aux : 'a . 'a seq -> int -> int -> int * 'a = 
        fun xs i max ->
          match xs with
            Nil -> failwith "index out of bounds"
          | Zero xs -> 
              let i, (a, b) = aux xs i (2 * max) in
              if i > (max - 1) then (i - max), b else i, a
          | One (x, xs) -> 
              if i < max 
              then i, x
              else 
                let i, (a, b) = aux xs (i - max) (2 * max) in
                if i > (max - 1) then (i - max), b else i, a
      in
      let _, x = aux xs i 1 in
      x 
  
  let rec map : 'a . ('a -> 'a) -> 'a seq -> 'a seq =
    fun f xs ->
      match xs with
        Nil -> Nil
      | Zero xs -> Zero (map (fun (a,b) -> (f a, f b)) xs)
      | One (x, xs) -> One (f x, map (fun (a,b) -> (f a, f b)) xs)
    
  let fupdate : 'a . int -> ('a -> 'a) -> 'a seq -> 'a seq =
    fun i f xs ->
      let rec aux : 'a . int -> (int -> 'a -> (int * 'a)) -> 'a seq -> 'a seq = 
        fun idx f xs ->
          match xs with
            Nil -> Nil
          | Zero xs -> Zero 
                         (aux 
                            idx
                            (fun idx (a, b) -> 
                               let idx, a = f idx a in
                               let idx, b = f idx b in
                               idx, (a, b)
                            ) 
                            xs 
                         )
          | One (x, xs) ->
              let idx, x = f idx x in
              One (x, 
                   aux 
                     idx
                     (fun idx (a, b) -> 
                        let idx, a = f idx a in
                        let idx, b = f idx b in
                        idx, (a, b)
                     ) 
                     xs
                  )
      in
      aux 0 
        (fun idx a -> 
           let a = if idx = i then f a else a in
           idx+1, a
        ) xs
  
  let update : 'a . int -> 'a -> 'a seq -> 'a seq =
    fun i a xs -> fupdate i (fun _ -> a) xs
  
  (* An application of random access lists. *)
  
  let rec eval (env : env) (e : expr) : constant =
    match e with
      EConstant c -> c
    | EBinOp (e1, op, e2) -> op (eval env e1) (eval env e2)
    | EVar v -> get v env
    | ELet (e1, e2) -> 
        let env = cons (eval env e1) env in
        eval env e2
  