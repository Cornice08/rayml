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
  if Float.(Float.abs ray.direction.{2} < 0.0001) then 
    [] 
  else
    let t = -.ray.origin.{2} /. ray.direction.{2} in 
    [t]
    

let normal_at () = Vector.make 0. 1. 0. 