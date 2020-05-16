-- ==
-- input @ two_100_f32s
-- input @ two_1000_f32s
-- input @ two_10000_f32s
-- input @ two_100000_f32s
-- input @ two_1000000_f32s

let f ( x : f32 ) ( y : f32 ) : f32 =
    2*x*x*x*x*x*x*y*y - x*x*x*x*x*x*y + 3*x*x*x*y*y*y - x*x*y*y*y + x*x*x*y - 3*x*y*y + x*y - 5*y + 2*x*x*x*x*x*y*y*y*y - 2*x*x*x*x*x*y*y*y*y*y + 250

let integrate [n] (xs: [n]f32) (ys: [n]f32) : f32 = (reduce (+) 0.0 (map2 f xs ys))*4.0 / r32 n


let main (xs: []f32) (ys: []f32) = integrate xs ys
