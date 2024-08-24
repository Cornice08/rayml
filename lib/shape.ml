open! Core 
open Option.Let_syntax

type t = 
  | Sphere of Sphere.t 
  | Plane of Plane.t 

type intersection = {
  t: float; 
  obj: t
}

let get_inverse_transform shape = 
  match shape with 
  | Sphere { inverse_transform; _ } -> inverse_transform
  | Plane { inverse_transform; _ } -> inverse_transform

let intersect (shape: t) ray  = 
  let ray = Ray.transform ray (get_inverse_transform shape) in

  let intersections = match shape with 
    | Sphere sphere -> 
        Sphere.local_intersect sphere ray 
    | Plane _ -> Plane.local_intersect ray
  in 
  List.map ~f:(fun f -> { t=f; obj=shape }) intersections


type hit_record = {
  t: float; 
  point: Point.t; 
  normal: Vector.t; 
  eye: Vector.t; 
  material: Material.t; 
  inside: bool;
  over_point: Point.t;
}

let local_normal_at shape point = 
  match shape with 
  | Sphere _ -> Sphere.normal_at point
  | Plane _ -> Plane.normal_at ()

let normal_at shape point = 
  let inverse = get_inverse_transform shape in 
  let obj_point = Matrix.transform_point inverse point in 
  let obj_normal = local_normal_at shape obj_point in

  let world_normal = Matrix.transform_vector (Matrix.transpose inverse) obj_normal in 

  Vector.normalize world_normal

let get_material shape = 
  match shape with 
  | Sphere {material; _} -> material
  | Plane {material; _ } -> material
        
let hit (intersections: intersection list) ray = 
  let candidates = List.filter ~f:(fun x -> Float.(x.t >= 0.)) intersections in 
  let hit_opt = List.min_elt ~compare:(fun x y -> Float.compare x.t y.t) candidates in 
  let%bind hit = hit_opt in 
  let point = Ray.position ray hit.t in
  let normal = normal_at hit.obj point in 
  let eye = Vector.neg ray.direction in
  let (normal, inside) = if Float.((Vector.dot normal eye) < 0.) then 
    (Vector.neg normal, true) 
  else
    (normal, false)
  in 
  Some {
    t = hit.t;
    material = get_material hit.obj; 
    point; 
    normal; 
    eye;
    inside;
    over_point = Vector.add point (Vector.scalar_mul normal 0.0001)
  }

let%expect_test "intersects at two points" = 
  let ray = Ray.make (Point.make 0. 0. (-5.)) (Vector.make 0. 0. 1.) in 
  let sphere = Sphere.unit () in 

  let intersections = intersect (Sphere sphere) ray in 

  Format.printf "%s" (List.to_string ~f:(fun (x:intersection) -> Printf.sprintf "%f" x.t) intersections);
  [%expect {| (4.000000 6.000000) |}];
;;

let%expect_test "intersects at tangent" = 
  let ray = Ray.make (Point.make 0. 1. (-5.)) (Vector.make 0. 0. 1.)  in 
  let sphere = Sphere.unit () in 

  let intersections = intersect (Sphere sphere) ray in 

  Format.printf "%s" (List.to_string ~f:(fun (x:intersection) -> Printf.sprintf "%f" x.t) intersections);
  [%expect {| (5.000000 5.000000) |}];
;;

let%expect_test "no intersection" = 
  let ray = Ray.make (Point.make 0. 0. 0.) (Vector.make 0. 0. 1.) in 
  let sphere = Sphere.unit () in 

  let intersections = intersect (Sphere sphere) ray in 

  Format.printf "%s" (List.to_string ~f:(fun (x:intersection) -> Printf.sprintf "%f" x.t) intersections);
  [%expect {| (-1.000000 1.000000) |}];
;;

let%expect_test "sphere behind ray" = 
  let ray = Ray.make (Point.make 0. 0. 5.) (Vector.make 0. 0. 1.) in 
  let sphere = Sphere.unit () in 

  let intersections = intersect (Sphere sphere) ray in 

  Format.printf "%s" (List.to_string ~f:(fun (x:intersection) -> Printf.sprintf "%f" x.t) intersections);
  [%expect {| (-6.000000 -4.000000) |}];
;;

let%expect_test "intersecting scaled sphere" = 
  let ray = Ray.make (Point.make 0. 0. (-5.)) (Vector.make 0. 0. 1.) in 
  let sphere = Sphere.unit () in 
  let scaling = Matrix.scaling 2.0 in 
  let sphere = {sphere with inverse_transform = scaling |> Matrix.inverse} in 

  let intersections = intersect (Sphere sphere) ray in 

  Format.printf "%s" (List.to_string ~f:(fun (x:intersection) -> Printf.sprintf "%f" x.t) intersections);
  [%expect {| (3.000000 7.000000) |}];
;;

let%expect_test "intersecting translated sphere" = 
  let ray = Ray.make (Point.make 0. 0. (-5.)) (Vector.make 0. 0. 1.) in 
  let sphere = Sphere.unit () in 
  let translation = Matrix.translation 5.0 0. 0. in 
  let sphere = {sphere with inverse_transform = translation |> Matrix.inverse} in 

  let intersections = intersect (Sphere sphere) ray in 

  Format.printf "%s" (List.to_string ~f:(fun (x:intersection) -> Printf.sprintf "%f" x.t) intersections);
  [%expect {| () |}];
;;

  
let%expect_test "hit record when multiple positive t" = 
  let ray = Ray.make (Point.make 0. 0. (-5.)) (Vector.make 0. 0. 1.) in 
  let sphere = Sphere.unit () in 

  let intersections = intersect (Sphere sphere) ray in 
  let hit = hit intersections ray in 
  let _ = match hit with 
  | Some hit -> Format.printf "%f" hit.t;
  | _ -> Printf.printf "Invalid";
  in

  [%expect {| 4.000000 |}]
;;




let%expect_test "normal of point on x axis" = 
  let sphere = Sphere.unit () in 
  let normal = normal_at (Sphere sphere) (Point.make 1. 0. 0.) in

  Format.printf "%a" Vector.pp normal;

  [%expect {| [1.; 0.; 0.] |}]
;;

let%expect_test "normal of point on y axis" = 
  let sphere = Sphere.unit () in 
  let normal = normal_at (Sphere sphere) (Point.make 0. 1. 0.) in

  Format.printf "%a" Vector.pp normal;

  [%expect {| [0.; 1.; 0.] |}]
;;

let%expect_test "normal of point on z axis" = 
  let sphere = Sphere.unit () in 
  let normal = normal_at (Sphere sphere) (Point.make 0. 0. 1.) in

  Format.printf "%a" Vector.pp normal;

  [%expect {| [0.; 0.; 1.] |}]
;;

let%expect_test "normal of point on z axis" = 
  let sphere = Sphere.unit () in 
  let normal = normal_at (Sphere sphere) (Point.make 0. 0. 1.) in

  Format.printf "%a" Vector.pp normal;

  [%expect {| [0.; 0.; 1.] |}]
;;

