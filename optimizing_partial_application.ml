let ccr = fun a -> 
  let ca = cos (a/.2.) *. 8. in
  fun b -> 
    let cb = cos (b/.2.) *. ca in
    fun c -> 
      let cc = cos (c/.2.) *. cb in
      fun s ->
        s /. cc

