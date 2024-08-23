open Lacaml.D
open Core 

type t = vec

let make r g b = Vec.of_list [r; g; b]

let black = Vec.make 3 0.0

let white = Vec.make 3 1.0

let to_ppm color = 
  let r = Float.clamp_exn (color.{1} *. 255.) ~min:0. ~max:255. in 
  let g = Float.clamp_exn (color.{2} *. 255.) ~min:0. ~max:255. in 
  let b = Float.clamp_exn (color.{3} *. 255.) ~min:0. ~max:255. in 
  Printf.sprintf "%d %d %d" (Float.to_int r) (Float.to_int g) (Float.to_int b)