open! Core 

type t = Vector.t

let make r g b = Vector.make r g b

let black = Vector.make 0.0 0.0 0.0

let white = Vector.make 1.0 1.0 1.0

let to_ppm color = 
  let (r, g, b) = Vector.elements color in
  let r = Float.clamp_exn (r *. 255.) ~min:0. ~max:255. in 
  let g = Float.clamp_exn (g *. 255.) ~min:0. ~max:255. in 
  let b = Float.clamp_exn (b *. 255.) ~min:0. ~max:255. in 
  Printf.sprintf "%d %d %d" (Float.to_int r) (Float.to_int g) (Float.to_int b)