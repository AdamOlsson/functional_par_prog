import "ising"
import "lib/github.com/diku-dk/lys/lys"

type text_content = (i32, f32, f32, i32)
module lys: lys with text_content = text_content = {
  type~ state = { abs_temp: f32
                , samplerate: f32
                , height: i32
                , width: i32
                , grid: ([][]rng_engine.rng, [][]spin)
                }

  let init (seed: u32) (height: i32) (width: i32): state =
    {abs_temp = 0.2, samplerate = 0.025,
     grid = random_grid (i32.u32 seed) height width,
     height, width}

  let resize (height: i32) (width: i32) (s: state) =
    s with height = height
      with width = width
      with grid = random_grid (delta_sum s.grid.1) height width

  let keydown k (s: state) =
    if      k == SDLK_RIGHT
    then s with samplerate = s.samplerate * 1.01
    else if k == SDLK_LEFT
    then s with samplerate = s.samplerate * 0.99
    else if k == SDLK_UP
    then s with abs_temp = s.abs_temp * 1.01
    else if k == SDLK_DOWN
    then s with abs_temp = s.abs_temp * 0.99
    else s

  let grab_mouse = false

  let same_2d [n] [m] 'a 'b (_: [n][m]a) (B: [][]b) : [n][m]b =
    B :> [n][m]b

  let step (s: state) =
    let rngs = s.grid.0
    let spin = same_2d rngs (s.grid.1)
    in s with grid = step s.abs_temp s.samplerate rngs spin

  let event (e: event) (s: state) : state =
    match e
    case #step _ -> step s
    case #keydown {key} -> keydown key s
    case _ -> s

  let render (s: state) =
    render s.grid.1

  type text_content = text_content
  let text_format () = "FPS: %d\nTemperature: %f (up/down to change)\nSamplerate: %f (left/right to change)\nOrdering: %d"
  let text_colour _ = argb.white
  let text_content fps (s: state) =
    (t32 fps, s.abs_temp, s.samplerate, delta_sum s.grid.1)
}
