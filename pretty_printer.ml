(* The type of requirements. *)

type req =
  | Infinite
  | Finite of int (* always nonnegative *)

(* The type of documents. *)

type doc =
  | Empty
  | HardLine
  | Char of char (* never '\n' *)
  | Cat of req * doc * doc
  | Nest of int * req * doc
  | Group of req * doc
  | IfFlat of doc (* never [IfFlat _] *) * doc

(* The internal state of the rendering engine. *)

type state =
  {
    (* The line width. *)
    width: int;
    (* The current column. *)
    mutable column: int;
    (* The output buffer. *)
    output: Buffer.t;
  }

(* Addition of requirements. *)

let (++) (req1 : req) (req2 : req) : req =
  match req1, req2 with
  | Infinite, Infinite -> Infinite
  | Infinite, Finite _ -> Infinite
  | Finite _, Infinite -> Infinite
  | Finite m, Finite n -> Finite (m + n)

(* Comparison of requirements. *)

let (<==) (req1 : req) (req2 : req) : bool =
  match req1, req2 with
  | Infinite, Infinite -> true
  | Infinite, Finite _ -> false
  | Finite _, Infinite -> true
  | Finite m, Finite n -> m <= n
    
(* Determining the space requirement of a document. *)

(* This function is expected to run in constant time. *)

let rec requirement (doc : doc) : req =
  match doc with
  | Empty -> Finite 0
  | HardLine -> Infinite
  | Char _ -> Finite 1
  | Cat (r, _, _) -> r
  | Nest (_, r, _) -> r
  | Group (r, _) -> r
  | IfFlat (d, _) -> requirement d
  
(* Smart constructors. *)

let empty : doc =
  Empty

let hardline : doc =
  HardLine

let char (c : char) : doc =
  match c with
  | '\n' -> HardLine
  | c -> Char c

let (^^) (doc1 : doc) (doc2 : doc) : doc =
  let r1 = requirement doc1 in
  let r2 = requirement doc2 in
  let r = r1 ++ r2 in
  Cat (r, doc1, doc2)

let nest (i : int) (doc : doc) : doc =
  let r = requirement doc in
  Nest (i, r, doc)
  
let group (doc : doc) : doc =
  let r = requirement doc in
  Group (r, doc)

let ifflat (doc1 : doc) (doc2 : doc) : doc =
  match doc1 with
  | IfFlat (doc1, _) -> IfFlat (doc1, doc2)
  | doc1 -> IfFlat (doc1, doc2) 

let rec fix_indent state (indent : int) =
  if indent = 0 then ()
  else begin
    state.column <- state.column + 1;
    Buffer.add_char state.output ' ';
    fix_indent state (indent - 1)
  end
  
  
let rec render state (indent : int) (flatten : bool) doc =
  match doc with
  | Empty -> ()
  | HardLine ->
      Buffer.add_char state.output '\n';
      fix_indent state indent; 
      state.column <- indent
  | Char c ->
      Buffer.add_char state.output c;
      state.column <- state.column + 1
  | Cat (_, doc1, doc2) ->
      render state indent flatten doc1;
      render state indent flatten doc2
  | Nest (n, _, doc) -> 
      render state (indent + n) flatten doc 
  | Group (Infinite, doc) -> render state indent false doc
  | Group (Finite n, doc) ->
      if flatten || state.column + n <= state.width 
      then render state indent true doc
      else render state indent false doc
  | IfFlat (doc1, doc2) ->
      if flatten 
      then render state indent flatten doc1
      else render state indent flatten doc2
        
let pretty width doc =
  let state = { width ; column = 0 ; output = Buffer.create 512 } in
  render state 0 false doc;
  Buffer.contents state.output 

