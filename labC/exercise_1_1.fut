-- ==
-- input @ two_100_i32s
-- input @ two_1000_i32s
-- input @ two_10000_i32s
-- input @ two_100000_i32s
-- input @ two_1000000_i32s
-- input @ two_5000000_i32s
-- input @ two_10000000_i32s
let s1 = [23 ,45 , -23 ,44 ,23 ,54 ,23 ,12 ,34 ,54 ,7 ,2 , 4 ,67]
let s2 = [ -2 , 3 , 4 ,57 ,34 , 2 , 5 ,56 ,56 , 3 ,3 ,5 ,77 ,89]

let process (xs: []i32) (ys: []i32): i32 =
  reduce i32.max 0 (map i32.abs (map2 (-) xs ys))

let main (xs: []i32) (ys: []i32) =
  process xs ys
