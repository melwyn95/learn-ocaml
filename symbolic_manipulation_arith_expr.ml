type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3))

let my_example = EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3))

let rec eval e =
  match e with
    EInt x -> x
  | EAdd (x, y) ->
      let x = eval x in
      let y = eval y in
      x + y
  | EMul (x, y) ->
      let x = eval x in
      let y = eval y in
      x * y

let factorize e =
  match e with
    EAdd (EMul (a, b), EMul (d, c)) when a = d ->
      EMul (a, EAdd (b, c))
  | _ -> e

let expand e =
  match e with
    EMul (a, EAdd (b, c)) ->
      EAdd (EMul (a, b), EMul (a, c))
  | _ -> e

let simplify e =
  match e with
    EMul (a, EInt 0)
  | EMul (EInt 0, a) -> EInt 0
  | EMul (a, EInt 1)
  | EMul (EInt 1, a) -> a
  | EAdd (a, EInt 0)
  | EAdd (EInt 0, a) -> a
  | _ -> e