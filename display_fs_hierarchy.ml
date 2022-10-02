type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list

let rec print_path path = 
  print_string @@ String.concat "/" path

let rec print_file lvl name =
  let rec aux lvl =
    if lvl = 0 then () else
      let () = print_string "| " in
      aux (lvl - 1)
  in
  let () = aux lvl in
  print_string name
  
let rec print_symlink lvl name path =
  let rec aux lvl =
    if lvl = 0 then () else
      let () = print_string "| " in
      aux (lvl - 1)
  in
  let () = aux lvl in
  let () = print_string name in
  let () = print_string " -> " in
  print_path path
    
let rec print_dir lvl name =
  let rec aux lvl =
    if lvl = 0 then () else
      let () = print_string "| " in
      aux (lvl - 1)
  in
  let () = aux lvl in
  let () = print_string "/" in
  print_string name

let print_filesystem root =
  let rec print_filesystem lvl items =
    match items with
      [] -> ()
    | (name, File)::fs -> 
        let () = print_file lvl name in
        let () = print_newline() in
        print_filesystem lvl fs
    | (name, Dir fs')::fs -> 
        let () = print_dir lvl name in
        let () = print_newline() in
        let () = print_filesystem (lvl + 1) fs' in
        print_filesystem lvl fs
    | (name, Symlink path)::fs -> 
        let () = print_symlink lvl name path in
        let () = print_newline() in
        print_filesystem lvl fs
  in
  print_filesystem 0 root ;;

let rec resolve sym path = 
  let rec resolve acc path =
    match path with
      [] -> List.rev acc 
    | ".."::path when acc = [] -> resolve acc path
    | dir::path when acc = [] -> resolve [dir] path
    | ".."::path -> resolve (List.tl acc) path 
    | dir::path -> resolve (dir::acc) path
  in
  resolve (List.tl (List.rev sym)) path ;; 

let rec file_exists_cwd fs name =
  match fs with
    [] -> false
  | (name',File)::_ when name = name' -> true
  | (name',Symlink _)::_ when name = name' -> true
  | _::fs -> file_exists_cwd fs name 

let rec find_dir_cwd fs name =
  match fs with
    [] -> None
  | (name',Dir fs)::_ when name = name' -> Some fs
  | _::fs -> find_dir_cwd fs name

let rec file_exists root path =
  match path with 
    [name] -> file_exists_cwd root name
  | dir::path ->
      (match find_dir_cwd root dir with
         Some fs -> file_exists fs path
       | None -> false)
  | _ -> false  

let print_filesystem root =
  let rec print_filesystem sofar lvl items =
    match items with
      [] -> ()
    | (name, File)::fs -> 
        let () = print_file lvl name in
        let () = print_newline() in
        print_filesystem sofar lvl fs
    | (name, Dir fs')::fs ->
        let () = print_dir lvl name in
        let () = print_newline() in
        let () = print_filesystem (name::sofar) (lvl + 1) fs' in
        print_filesystem sofar lvl fs
    | (name, Symlink path)::fs -> 
        let () = if file_exists root (resolve (List.rev (name::sofar)) path) 
          then print_symlink lvl name path
          else print_symlink lvl name ["INVALID"] 
        in
        let () = print_newline() in
        print_filesystem sofar lvl fs
  in
  print_filesystem [] 0 root ;;