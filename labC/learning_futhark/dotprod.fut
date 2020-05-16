let main (x: []i32) (y: []i32): i32 =
    reduce (+) 0 (map2 (*) x y)

let fib (n: i32): []i32 =
  -- Create "empty" array.
  let arr = replicate n 0
  -- Fill array with Fibonacci numbers.
  in loop (arr) for i < n-2 do
       arr with [i+2] = arr[i] + arr[i+1]