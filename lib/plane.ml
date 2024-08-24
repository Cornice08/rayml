open! Core 

type t = {
  transform: Matrix.t; 
  material: Material.t; 
}

let make () = { 
  transform=Matrix.identity; 
  material=Material.default ();
}

let local_intersect (ray: Ray.t) = 
  let (_, dir_y, _) = Vector.elements ray.direction in 
  let (_, o_y, _) = Vector.elements ray.origin in
  if Float.(Float.abs dir_y < 0.0001) then 
    [] 
  else
    let t = -.o_y /. dir_y in 
    [t]
    

let normal_at () = Vector.make 0. 1. 0. 