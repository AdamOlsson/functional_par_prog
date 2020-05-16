-- ==
-- input @ pair_100_i32_bools
-- input @ pair_1000_i32_bools
-- input @ pair_10000_i32_bools
-- input @ pair_100000_i32_bools
-- input @ pair_1000000_i32_bools
-- input @ pair_5000000_i32_bools
-- input @ pair_10000000_i32_bools

let main [n] (k:[n]i32) (d:[n]bool)= reduce (+) 0 k