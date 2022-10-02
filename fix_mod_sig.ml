module type DictSig = sig
  type ('key, 'value) t
  val empty : ('key, 'value) t
  val add : ('key, 'value) t -> 'key -> 'value -> ('key, 'value) t
  exception NotFound
  val lookup : ('key, 'value) t -> 'key -> 'value
  val remove : ('key, 'value) t -> 'key -> ('key, 'value) t
end ;;

module Dict : DictSig = struct
  type ('key, 'value) t =
    | Empty
    | Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t

  let empty = Empty

  let rec add d k v =
    match d with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (l, k', v', r) ->
        if k = k' then Node (l, k, v, r)
        else if k < k' then Node (add l k v, k', v', r)
        else Node (l, k', v', add r k v)

  exception NotFound

  let rec lookup d k =
    match d with
    | Empty ->
        raise NotFound
    | Node (l, k', v', r) ->
        if k = k' then v'
        else if k < k' then lookup l k
        else lookup r k

  let rec fold f d a =
    match d with
      Empty -> a
    | Node (l, k, v, r) ->
        let d = f a k v in
        let d = fold f l d in
        let d = fold f r d in
        d
  
  let rec remove d k =
    match d with
    | Empty -> Empty
    | Node (l, k', v', r) ->
        if k = k' then fold add l r
        else if k < k' then Node (remove l k, k', v', r)
        else Node (l, k', v', remove r k)
  
end ;;
