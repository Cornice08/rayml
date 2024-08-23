open! Core

type t = {
  center: Point.t; 
  transform: Matrix.t; 
  material: Material.t 
}

let make center transform material = 
  {
    center;
    transform;
    material;
  }

let unit () = 
  make Point.origin Matrix.identity (Material.default ())

let local_intersect sphere (ray: Ray.t) : float list = 
  let sphere_to_ray = Vector.sub ray.origin sphere.center in 
  
  let a = Vector.dot ray.direction ray.direction in
  let b = 2.0 *. (Vector.dot ray.direction sphere_to_ray) in
  let c = (Vector.dot sphere_to_ray sphere_to_ray) -. 1. in 

  let discriminant = b *. b -. 4. *. a *. c in 

  let intersections = if Float.(discriminant < 0.) then 
    [] 
  else
    let t1 = (-.b -. (Float.sqrt discriminant)) /. (2. *. a) in 
    let t2 = (-.b +. (Float.sqrt discriminant)) /. (2. *. a) in 
    [
      t1; t2;
    ]
  in 
  intersections

let normal_at point = 
  Vector.sub point Point.origin
