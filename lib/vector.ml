open Lacaml.D
include Lacaml.D.Vec

type t = vec

let make x y z = Vec.of_list [x; y; z]

let pp_vec = pp_vec

let scalar_mul v x = 
  Vec.map (fun e -> e *. x) v

let scalar_div v x = 
  Vec.map (fun e -> e /. x) v

let magnitude v = 
  let sqrd = Vec.sqr v in 
  let sum = Vec.sum sqrd in 
  Float.sqrt sum

let normalize v : t = 
  let mag = magnitude v in
  Vec.map (fun e -> e /. mag) v

let dot a b = 
  Vec.sum (Vec.mul a b)

let cross a b = 
 let ax = a.{1} in 
 let ay = a.{2} in 
 let az = a.{3} in 
 let bx = b.{1} in 
 let by = b.{2} in 
 let bz = b.{3} in 

 Vec.of_list [
  ay *. bz -. az *. by; 
  az *. bx -. ax *. bz; 
  ax *. by -. ay *. bx
]

let reflect_about_normal v ~normal = 
  Vec.sub v (scalar_mul normal (2. *. dot v normal))


let%expect_test "reflect 45 degree vector" = 
  let v = make 1. (-1.) 0. in 
  let normal = make 0. 1. 0. in 

  let reflect = reflect_about_normal v ~normal in 

  Format.printf "%a" pp_vec reflect;

  [%expect {|
    1
    1
    0
    |}]
;;