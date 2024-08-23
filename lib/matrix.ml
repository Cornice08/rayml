open Lacaml.D 

type t = mat 

let identity = Mat.identity 4


let translation x y z = 
  Mat.of_list [
    [1.; 0.; 0.; x];
    [0.; 1.; 0.; y];
    [0.; 0.; 1.; z];
    [0.; 0.; 0.; 1.];
  ]

let rotation_x r = 
  Mat.of_list [
    [1.; 0.; 0.; 0.];
    [0.; Float.cos r; -.Float.sin r; 0.];
    [0.; Float.sin r; Float.cos r; 0.];
    [0.; 0.; 0.; 1.];
  ]

let rotation_y r = 
  Mat.of_list [
    [Float.cos r; 0.; Float.sin r; 0.];
    [0.; 1.; 0.; 0.];
    [-.Float.sin r; 0.; Float.cos r; 0.];
    [0.; 0.; 0.; 1.];
  ]

  let non_uniform_scaling x y z = 
    Mat.of_list [
      [x; 0.; 0.; 0.];
      [0.; y; 0.; 0.];
      [0.; 0.; z; 0.];
      [0.; 0.; 0.; 1.];
    ]

  let scaling f = non_uniform_scaling f f f

  let matrix_mul m1 m2 = 
    gemm m1 m2

  let sequence transforms = 
    List.fold_left (fun acc m -> matrix_mul acc m) identity transforms
    

  let transform_point matrix point = 
    let tmp_vec = Vec.make0 4 in 
    tmp_vec.{1} <- point.{1};
    tmp_vec.{2} <- point.{2};
    tmp_vec.{3} <- point.{3};
    tmp_vec.{4} <- 1.;
    let tmp_mat = Mat.of_col_vecs_list [tmp_vec] in
    let res = matrix_mul matrix tmp_mat in 
    let tmp_vec = Mat.as_vec res in 
    let vec = Vec.make0 3 in 
    vec.{1} <- tmp_vec.{1};
    vec.{2} <- tmp_vec.{2};
    vec.{3} <- tmp_vec.{3};
    vec

  let transform_vector matrix vec = 
    let tmp_vec = Vec.make0 4 in 
    tmp_vec.{1} <- vec.{1};
    tmp_vec.{2} <- vec.{2};
    tmp_vec.{3} <- vec.{3};
    let tmp_mat = Mat.of_col_vecs_list [tmp_vec] in
    let res = matrix_mul matrix tmp_mat in 
    let tmp_vec = Mat.as_vec res in 
    let vec = Vec.make0 3 in 
    vec.{1} <- tmp_vec.{1};
    vec.{2} <- tmp_vec.{2};
    vec.{3} <- tmp_vec.{3};
    vec
    

  let inverse matrix = 
    let tmp = Mat.map (fun x -> x) matrix in
    getri tmp;
    tmp

  let transpose matrix = 
    Mat.transpose_copy matrix


  let view_transform (from : Point.t) (to_: Point.t) (up: Vector.t) = 
    let forward = Vector.sub to_ from |> Vector.normalize in 
    let up = Vector.normalize up in 
    let left = Vector.cross forward up in 
    let true_up = Vector.cross left forward in 

    let orientation = Mat.of_list [
      [left.{1}; left.{2}; left.{3}; 0.];
      [true_up.{1}; true_up.{2}; true_up.{3}; 0.];
      [-.forward.{1}; -.forward.{2}; -.forward.{3}; 0.];
      [0.; 0.; 0.; 1.]
    ] in 

    let neg_from = Vector.neg from in
    let trans = translation neg_from.{1} neg_from.{2} neg_from.{3} in 

    sequence [orientation; trans]

  let%expect_test "view transform moves the worldr" = 
    let from = Point.make 0. 0. 8. in 
    let to_ = Point.make 0. 0. 0. in 
    let up = Vector.make 0. 1. 0. in 

    let transform = view_transform from to_ up in 
    Format.printf "%a" pp_mat transform;
  
    [%expect {|
      1 0 0  0
      0 1 0  0
      0 0 1 -8
      0 0 0  1
      |}]
  ;;