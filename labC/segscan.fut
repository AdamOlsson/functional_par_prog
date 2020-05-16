-- ==
-- input @ pair_100_i32_bools
-- input @ pair_1000_i32_bools
-- input @ pair_10000_i32_bools
-- input @ pair_100000_i32_bools
-- input @ pair_1000000_i32_bools
-- input @ pair_5000000_i32_bools
-- input @ pair_10000000_i32_bools

let oper 't (op: t -> t -> t) ((v1,f1): (t, bool)) ((v2,f2): (t, bool)) : (t, bool) =
    if f2 then (v2   , f1 || f2)
    else       (op v1 v2, f1 || f2)


let segscan [n] 't (op: t -> t -> t) (ne :t) (arr: [n](t, bool)) : [n]t =
                    map (\(v,_)-> v) (scan (oper op) (ne, false) arr)


let main [n] (k:[n]i32) (d:[n]bool) = segscan (+) 0 (zip k d)
