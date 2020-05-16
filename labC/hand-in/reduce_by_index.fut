-- ==
-- input @ two_100_i32s
-- input @ two_1000_i32s
-- input @ two_10000_i32s
-- input @ two_100000_i32s
-- input @ two_1000000_i32s
-- input @ two_5000000_i32s
-- input @ two_10000000_i32s

import "lib/github.com/diku-dk/sorts/radix_sort"

let segment0 = [(1,false),(2,false),(3,false),(4,true),(5,false),(6,true),(7,false),(8,true),(9,false),(10,true)]
let segment1 = [(2,false),(3,false),(1,false),(5,true),(4,false),(7,true),(6,false),(9,true),(8,false),(10,true)]
let segment2 = [(5, false), (2, false), (3, false), (9, false), (5, false), (4, true), (1, false), (8, true), (2, false)]
let segment3 = [(5, false), (2, false), (5, false), (9, false), (3, true),  (4, false),(1, true),  (2,false), (8, false)]


let data = [5,2,2,8,5,4,1,9,3]
let keys = [1,1,3,3,1,2,2,1,1]

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


let reduce_by_index 'a [n] (f: a -> a -> a) (ne: a) (is: [n]i32) (as: [n]a) : []a =
    let key    = (\(k,_) -> k)
    let sorted = radix_sort_by_key key i32.num_bits i32.get_bit (zip is as)
    let (k, d) = unzip sorted
    let cmp x y = if y - x >= 0 then false else true  
    let flags  = map2 cmp k (rotate (n-1) k)
    in segreduce f ne (zip d flags)


let main [n] (k:[n]i32) (d:[n]i32)= reduce_by_index (+) 0 k d