(* Prelude *)

(* We refer to the input data (which we wish to compress) as "text". *)

type text =
  string

(* A Huffman tree is a binary tree whose leaves carry a character. *)

type tree =
| Leaf of char
| Node of tree * tree

(* As a simple-minded representation of binary data, we use strings
   made up exclusively of the characters '0' and '1'. *)

type data =
  string

(* An alphabet maps characters to integer frequencies. *)

type alphabet =
  (char, int) Hashtbl.t

(* An encoding dictionary maps input characters to binary strings. *)

type encoding_dictionary =
  (char, data) Hashtbl.t

(* A decoding dictionary is a Huffman tree. *)

type decoding_dictionary =
  tree

(* Sorting. *)

let sort : char list -> char list =
  List.sort compare

(* The leaves of a tree. *)

let leaves tree =
  let rec leaves tree accu =
    match tree with
    | Leaf c ->
        c :: accu
    | Node (tree0, tree1) ->
        leaves tree0 (leaves tree1 accu)
  in
  leaves tree []

(* [entries table] produces a list of the key-value pairs found in the hash
   table [table]. The list is sorted by key using OCaml's generic comparison
   function [compare]. *)

let entries table =
  Hashtbl.fold (fun key value entries ->
    (key, value) :: entries
  ) table []
  |> List.sort (fun (key1, _) (key2, _) -> compare key1 key2)

(* [write_char b c] converts the character [c] to a sequence of 8 binary
   characters (each of which is '0' or '1'). This sequence is written to
   the buffer [b]. *)

let write_char (b : Buffer.t) (c : char) =
  let c = ref (Char.code c) in
  for i = 0 to 7 do
    Buffer.add_char b (if !c land 0x80 = 0 then '0' else '1');
    c := !c lsl 1
  done

(* Assuming that the function [next] is a source of characters (i.e., every
   time it is called, it produces the next character, read from some source)
   [read_char next] reads 8 binary characters (each of which is '0' or '1')
   and combines them to produce a single character. It is the inverse of
   [write_char] above. *)

let read_char (next : unit -> char) : char =
  let c = ref 0 in
  let mask = ref 0x80 in
  for i = 0 to 7 do
    if next() = '1' then c := !c lor !mask;
    mask := !mask lsr 1
  done;
  Char.chr !c

(* -------------------------------------------------------------------------- *)

(* Building an alphabet out of a piece of text. *)

let build_alphabet (text : text) : alphabet =
  let h : alphabet = Hashtbl.create 1024 in
  let () = String.iter (fun c -> 
      if Hashtbl.mem h c
      then Hashtbl.replace h c (1 + (Hashtbl.find h c))
      else Hashtbl.add h c 1) text
  in
  h

(* -------------------------------------------------------------------------- *)

(* Building encoding and decoding dictionaries out of an alphabet. *)

module T = struct
  type t = (int * tree)
  let compare = (fun (x, _) (y, _) -> compare x y)
end

module Q = BinomialQueue(T);;

let build_tree (alphabet : alphabet) : tree =
  (* Assumption: the alphabet has at least two symbols. *)
  assert (Hashtbl.length alphabet >= 2);
  let q = Hashtbl.fold (fun a c q ->  Q.insert (c, Leaf a) q) alphabet Q.empty in
  let rec aux q = 
    let (c1, t1), q = Q.extract q in
    if Q.is_empty q
    then t1
    else 
      let (c2, t2), q = Q.extract q in
      let q = Q.insert ((c1+c2), Node (t1, t2)) q in
      aux q
  in
  aux q
  
let build_encoding_dictionary (tree : tree) : encoding_dictionary =
  let dict : encoding_dictionary = Hashtbl.create 1024 in
  let rec aux t path =
    match t with
    | Leaf c -> Hashtbl.add dict c path
    | Node (l,r) -> 
        let () = aux l (path^"0") in
        let () = aux r (path^"1") in
        () 
  in
  let () = aux tree "" in
  dict
  
let build_dictionaries alphabet : encoding_dictionary * decoding_dictionary =
  let tree = build_tree alphabet in
  let encoding_dictionary = build_encoding_dictionary tree in
  let decoding_dictionary = tree in
  encoding_dictionary, decoding_dictionary

(* -------------------------------------------------------------------------- *)

(* Encoding input data. *)

let encode_char (dictionary : encoding_dictionary) (c : char) : data =
  try
    Hashtbl.find dictionary c
  with Not_found ->
    assert false (* unknown character *)

let encode (dictionary : encoding_dictionary) (text : text) : data =
  let buffer = Buffer.create 1024 in
  String.iter (fun c ->
      Buffer.add_string buffer (encode_char dictionary c)
    ) text;
  Buffer.contents buffer

(* -------------------------------------------------------------------------- *)

(* Decoding compressed data. *)

let rec find (data : data) (i : int) (t : tree) : char * int =
  assert (0 <= i && i <= String.length data);
  let rec aux i t =
    match t with
    | Leaf c -> (c,i)
    | Node (l,r) -> 
        if data.[i] = '0'
        then aux (i+1) l
        else aux (i+1) r
  in
  aux i t

let decode (tree : tree) (data : data) (i : int) : text =
  let buffer = Buffer.create 1024 in
  let rec loop i =
    if i = String.length data then
      (* We have reached the end of the data. We are done. *)
      Buffer.contents buffer
    else begin
      (* Decode one more character, and continue. *)
      let c, i = find data i tree in
      Buffer.add_char buffer c;
      loop i
    end
  in
  loop i

(* -------------------------------------------------------------------------- *)

(* Serializing a tree means encoding it as binary data.
   Here, this means encoding it as a string of '0' and '1' characters. *)

let write (tree : tree) : data =
  let b = Buffer.create 1024 in
  let rec write (tree : tree) : unit =
    match tree with
    | Leaf c -> 
        let () = Buffer.add_char b '0' in
        let () = write_char b c in
        ()
    | Node (l,r) ->
        let () = Buffer.add_char b '1' in
        let () = write l in
        let () = write r in 
        ()
  in
  write tree;
  Buffer.contents b

(* Deserializing a tree means reading its representation as binary data
   and transforming it back into a tree. *)

let read (s : data) : tree * int =
  let i = ref 0 in
  let next () : char =
    assert (!i < String.length s);
    let c = s.[!i] in
    incr i;
    c
  in
  let rec read () : tree =
    let c = s.[!i] in
    incr i;
    if c = '1'
    then 
      let l = read () in
      let r = read () in
      Node (l, r)
    else Leaf (read_char next)
  in
  let tree = read() in
  tree, !i

(* -------------------------------------------------------------------------- *)

(* Compressing and decompressing an input text. *)

let compress (text : text) : data =
  let alphabet = build_alphabet text in 
  let encoding_dictionary, decoding_dictionary = build_dictionaries alphabet in 
  let compressed_text = encode encoding_dictionary text in
  let decoding_dictionary = write decoding_dictionary in 
  decoding_dictionary ^ compressed_text 

let decompress (data : data) : text =
  let decoding_dictionary, i = read data in
  decode decoding_dictionary data i
