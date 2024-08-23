open Lacaml.D
open Core 

type t = {
  origin: Point.t;
  direction: vec;
}

let make origin direction = { origin; direction }

let position ray t : Point.t = 
  Vec.add ray.origin (Vector.scalar_mul ray.direction t)

let transform ray matrix : t = 
  let origin = Matrix.transform_point matrix ray.origin in 
  let direction = Matrix.transform_vector matrix ray.direction in 
  {
    origin;
    direction
  }

let%expect_test "position" = 
  let ray = make (Point.make 2. 3. 4.) (Vector.of_list [1.; 0.; 0.]) in 
  let point = position ray 0. in 
  Format.printf "%a" pp_vec point;
  [%expect {|
    2
    3
    4
    |}];

  let point = position ray 1. in 
  Format.printf "%a" pp_vec point;
  [%expect {|
    3
    3
    4
    |}];

  let point = position ray (-1.) in 
  Format.printf "%a" pp_vec point;
  [%expect {|
    1
    3
    4
    |}];

  let point = position ray 2.5 in 
  Format.printf "%a" pp_vec point;
  [%expect {|
    4.5
      3
      4
    |}];
;;


let%expect_test "translate ray" = 
  let ray = make (Point.make 1. 2. 3.) (Vec.of_list [0.; 1.; 0.]) in 
  let translate = Matrix.translation 3. 4. 5. in
  let new_ray = transform ray translate in 
  Format.printf "%a" pp_vec new_ray.origin;
  [%expect {|
    4
    6
    8
    |}];
    Format.printf "%a" pp_vec new_ray.direction;
  [%expect {|
    0
    1
    0
    |}];
;;

let%expect_test "scale ray" = 
  let ray = make (Point.make 1. 2. 3.) (Vec.of_list [0.; 1.; 0.]) in 
  let scaling = Matrix.non_uniform_scaling 2. 3. 4. in
  let new_ray = transform ray scaling in 
  Format.printf "%a" pp_vec new_ray.origin;
  [%expect {|
     2
     6
    12
    |}];
    Format.printf "%a" pp_vec new_ray.direction;
  [%expect {|
    0
    3
    0
    |}];
;;
