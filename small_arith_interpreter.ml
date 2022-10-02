type operation =
    Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list

let rec lookup_function n = function env ->
  match env with
    [] -> invalid_arg "lookup_function"
  | (f, fn)::_ when f = n -> fn
  | _::env -> lookup_function n env

let add_function name op env = (name, op)::env

let my_env = 
  [ ("min", fun x y -> if x < y then x else y) 
  ; ("add", ( + ))
  ; ("mul", ( * ))
  ; ("sub", ( - ))
  ; ("div", ( / ))
  ]

let rec compute env op =
  match op with
    Value v -> v
  | Op (f, x, y) ->
      let f = lookup_function f env in
      let x = compute env x in
      let y = compute env y in
      f x y

let rec compute_eff env = function
    Value v -> v
  | Op (f, x, y) -> 
      (((lookup_function f env) (compute_eff env x)) (compute_eff env y))