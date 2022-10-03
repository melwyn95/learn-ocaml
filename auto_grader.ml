type report = message list
and message = string * status
and status = Successful | Failed

type 'a result = Ok of 'a | Error of exn

let exec f x =
  match f x with
    v -> Ok v
  | exception e -> Error e
  
let compare user reference to_string =
  match user, reference with
    Ok x, Ok y when x = y 
    -> "got correct value " ^ to_string x, Successful
  | Error x, Error y when x = y 
    -> "got correct exception " ^ exn_to_string x, Successful
  | Error e, Error _
  | Error e, Ok _ 
    -> "got unexpected exception " ^ exn_to_string e, Failed
  | Ok v, Ok _
  | Ok v, Error _ 
    -> "got unexpected value " ^ to_string v, Failed

let test user reference sample to_string =
  let rec aux i report = 
    if i < 10 then 
      let s = sample () in
      let x = exec user s in
      let y = exec reference s in
      aux (i + 1) ((compare x y to_string) :: report)
    else List.rev report
  in
  aux 0 []