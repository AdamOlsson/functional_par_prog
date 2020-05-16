-- ==
-- input @ pair_100_i32_bools
-- input @ pair_1000_i32_bools
-- input @ pair_10000_i32_bools
-- input @ pair_100000_i32_bools
-- input @ pair_1000000_i32_bools
-- input @ pair_5000000_i32_bools
-- input @ pair_10000000_i32_bools

import "lib/github.com/diku-dk/sorts/radix_sort"

let segment0 = [(1,false),(2,false),(3,false),(4,true),(5,false),(6,true),(7,false),(8,true),(9,false),(10,true)]
let segment1 = [(2,false),(3,false),(1,false),(5,true),(4,false),(7,true),(6,false),(9,true),(8,false),(10,true)]
let segment2 = [(5, false), (2, false), (3, false), (9, false), (5, false), (4, true), (1, false), (8, true), (2, false)]

let oper 't (op: t -> t -> t) ((v1,f1): (t, bool)) ((v2,f2): (t, bool)) : (t, bool) =
    if f2 then (v2   , f1 || f2)
    else       (op v1 v2, f1 || f2)


let segscan [n] 't (op: t -> t -> t) (ne :t) (arr: [n](t, bool)) : [n]t =
                    map (\(v,_)-> v) (scan (oper op) (ne, false) arr)

let segreduce [n] 't  (op: t -> t -> t) (ne : t) (arr: [ n ](t, bool)) : []t =
    let (v, flags) = unzip arr
    let offsets   = scan (+) 0 (map i32.bool flags)
    let src = segscan op ne arr
    in (scatter (copy src) offsets src)[0:(offsets[(length offsets)-1])+1] -- remove non-relevant values


let main [n] (k:[n]i32) (d:[n]bool)= segreduce (+) 0 (zip k d)