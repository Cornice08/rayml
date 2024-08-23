open Lacaml.D

type t = vec

let make x y z = 
  Vec.of_list [x; y; z]

let origin = Vec.make0 3