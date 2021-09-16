let anagrams (s1 : string) (s2 : string) : bool = 
  let rec aux s i n xs = 
    if i = n
    then xs
    else aux s (i + 1) n (s.[i] :: xs) 
  in
  let xs = aux s1 0 (String.length s1) [] |> List.sort compare in
  let ys = aux s2 0 (String.length s2) [] |> List.sort compare in
  xs = ys