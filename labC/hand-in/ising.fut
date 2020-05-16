-- ==
-- input { 20.0f32 20.0f32 10i32 10i32 60i32 }
-- input { 20.0f32 20.0f32 100i32 100i32 60i32 }
-- input { 20.0f32 20.0f32 200i32 200i32 60i32 }
-- input { 20.0f32 20.0f32 200i32 200i32 50i32 }
-- input { 20.0f32 20.0f32 200i32 200i32 150i32 }
-- input { 20.0f32 20.0f32 200i32 200i32 250i32 }



-- futhark bench --backend=c ising.fut --json ising_bench-c.json

-- We represent a spin as a single byte.  In principle, we need only
-- two values (-1 or 1), but Futhark represents booleans a a full byte
-- entirely, so using an i8 instead takes no more space, and makes the
-- arithmetic simpler.
type spin = i8

import "lib/github.com/diku-dk/cpprandom/random"

-- Pick an RNG engine and define random distributions for specific types.
module rng_engine = minstd_rand
module rand_f32 = uniform_real_distribution f32 rng_engine
module rand_i8 = uniform_int_distribution i8 rng_engine

-- We can create an few RNG state with 'rng_engine.rng_from_seed [x]',
-- where 'x' is some seed.  We can split one RNG state into many with
-- 'rng_engine.split_rng'.
--
-- For an RNG state 'r', we can generate random integers that are
-- either 0 or 1 by calling 'rand_i8.rand (0i8, 1i8) r'.
--
-- For an RNG state 'r', we can generate random floats in the range
-- (0,1) by calling 'rand_f32.rand (0f32, 1f32) r'.
--
-- Remember to consult https://futhark-lang.org/docs/futlib/random.html

let rand = rand_f32.rand (0f32, 1f32)

-- Create a new grid of a given size.  Also produce an identically
-- sized array of RNG states.
let random_grid (seed: i32) (h: i32) (w: i32)
              : ([h][w]rng_engine.rng, [h][w]spin) =
  let rnd_1D = rng_engine.split_rng (h*w) (rng_engine.rng_from_seed [seed])
  let (rnd_gens, rnd_ints) = unzip (map (rand_i8.rand (0i8, 1i8)) rnd_1D)
  in (unflatten h w rnd_gens, unflatten h w rnd_ints)

-- Compute $\Delta_e$ for each spin in the grid, using wraparound at
-- the edges.
let deltas [h][w] (spins: [h][w]spin): [h][w]i8 =
  let delta c u d l r = 2*c*(u + d + l + r)
  let right xss = map (rotate (-1)) xss
  let left  xss = map (rotate   1)  xss
  let up    xss = rotate   1  xss
  let down  xss = rotate (-1) xss
  let map5_row c u d l r = map5 delta c u d l r
  in map5 map5_row spins (up spins) (down spins) (left spins) (right spins)

-- The sum of all deltas of a grid.  The result is a measure of how
-- ordered the grid is.
let delta_sum [h][w] (spins: [w][h]spin): i32 =
  deltas spins |> flatten |> map1 i32.i8 |> i32.sum

import "prelude/math"


let step [h][w] (abs_temp: f32) (samplerate: f32)
                (rngs: [h][w]rng_engine.rng) (spins: [h][w]spin)
              : ([h][w]rng_engine.rng, [h][w]spin) =
  let de = deltas spins
  let unzip_2D xss = (\(xs,ys)-> (unflatten h w xs, unflatten h w ys)) (unzip (flatten xss))
  let (as_gen, as_floats) = unzip_2D (map (\row -> map (rand_f32.rand (0f32, 1f32)) row) rngs)
  let (_,      bs_floats) = unzip_2D (map (\row -> map (rand_f32.rand (0f32, 1f32)) row) rngs)
  let to_float xs = map (map (\x -> f32.i8 x)) xs
  let c_prime p t a b de c = if a < p && (de < -de || b < f32.exp(-de/t)) then -(i8.f32 c) else (i8.f32 c)
  let map4_row ra rb rde rspin = map4 (c_prime samplerate abs_temp) ra rb rde rspin
  in (as_gen, map4 map4_row as_floats bs_floats (to_float de) (to_float spins))

import "lib/github.com/athas/matte/colour"

--  Turn a grid of spins into an array of pixel values, ready to be
-- blitted to the screen.
let render [h][w] (spins: [h][w]spin): [h][w]argb.colour =
  let pixel spin = if spin == -1i8
                   then argb.(bright <| light red)
                   else argb.(bright <| light blue)
  in map1 (map1 pixel) spins

--  Just for benchmarking.
let main (abs_temp: f32) (samplerate: f32)
         (h: i32) (w: i32) (n: i32): [h][w]spin =
  (loop (rngs, spins) = random_grid 1337 h w for _i < n do
     step abs_temp samplerate rngs spins).1
