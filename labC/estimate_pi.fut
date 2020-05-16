-- ==
-- input @ two_100_f32s
-- input @ two_10000_f32s
-- input @ two_1000000_f32s

let estimate_pi [n] (xs: [n]f32) (ys: [n]f32) : f32 =
    let in_circle x y = if ((x-1)*(x-1) + (y-1)*(y-1) <= 1) then 1 else 0
    let count = f32.i32 (reduce (+) 0 (map2 in_circle xs ys))
    in count/(r32 n) * 4


let main (xs: []f32) (ys: []f32) = estimate_pi xs ys