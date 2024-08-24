open! Core

type t = Vector.t list [@@deriving show] 

let identity = [
  [1.; 0.; 0.; 0.];
  [0.; 1.; 0.; 0.];
  [0.; 0.; 1.; 0.];
  [0.; 0.; 0.; 1.];
]


let translation x y z = 
  [
    [1.; 0.; 0.; x];
    [0.; 1.; 0.; y];
    [0.; 0.; 1.; z];
    [0.; 0.; 0.; 1.];
  ]

let rotation_x r = 
  [
    [1.; 0.; 0.; 0.];
    [0.; Float.cos r; -.Float.sin r; 0.];
    [0.; Float.sin r; Float.cos r; 0.];
    [0.; 0.; 0.; 1.];
  ]

let rotation_y r = 
  [
    [Float.cos r; 0.; Float.sin r; 0.];
    [0.; 1.; 0.; 0.];
    [-.Float.sin r; 0.; Float.cos r; 0.];
    [0.; 0.; 0.; 1.];
  ]

  let non_uniform_scaling x y z = 
    [
      [x; 0.; 0.; 0.];
      [0.; y; 0.; 0.];
      [0.; 0.; z; 0.];
      [0.; 0.; 0.; 1.];
    ]

  let scaling f = non_uniform_scaling f f f

  let transpose matrix = 
    List.transpose_exn matrix

  let matrix_mul m1 m2 = 
    let columns = transpose m2 in
    List.map ~f:(fun row -> List.map ~f:(Vector.dot row) columns) m1

  let sequence transforms = 
    List.fold_left ~f:(fun acc m -> matrix_mul acc m) ~init:identity transforms
    

  let transform_point matrix point = 
    let x, y, z = Vector.elements point in 
    let point_mat = [
      [x];
      [y];
      [z];
      [1.];
    ] in
    let res = matrix_mul matrix point_mat |> transpose in 
    let (vec, _) = List.split_n (List.hd_exn res) 3 in
    vec

  let transform_vector matrix vec = 
    let x, y, z = Vector.elements vec in 
    let vec_mat = [
      [x];
      [y];
      [z];
      [0.];
    ]in
    let res = matrix_mul matrix vec_mat |> transpose in 
    let (vec, _) = List.split_n (List.hd_exn res) 3 in
    vec
    

  let inverse matrix = 
    let tmp = Lacaml.D.Mat.of_list matrix in 
    Lacaml.D.getri tmp; 
    Lacaml.D.Mat.to_list tmp 

  let view_transform (from : Point.t) (to_: Point.t) (up: Vector.t) = 
    let forward = Vector.sub to_ from |> Vector.normalize in 
    let up = Vector.normalize up in 
    let left = Vector.cross forward up in 
    let true_up = Vector.cross left forward in 

    let (leftx, lefty, leftz) = Vector.elements left in 
    let (trux, truy, truz) = Vector.elements true_up in 
    let (forx, fory, forz) = Vector.elements forward in 

    let orientation = [
      [leftx; lefty; leftz; 0.];
      [trux; truy; truz; 0.];
      [-.forx; -.fory; -.forz; 0.];
      [0.; 0.; 0.; 1.]
    ] in 

    let (fromx, fromy, fromz) = Vector.elements from in
    let trans = translation (-.fromx) (-.fromy) (-.fromz) in 

    sequence [orientation; trans]

  let%expect_test "view transform moves the worldr" = 
    let from = Point.make 0. 0. 8. in 
    let to_ = Point.make 0. 0. 0. in 
    let up = Vector.make 0. 1. 0. in 

    let transform = view_transform from to_ up in 
    Format.printf "%a" pp transform;
  
    [%expect {| [[1.; 0.; 0.; 0.]; [0.; 1.; 0.; 0.]; [0.; 0.; 1.; -8.]; [0.; 0.; 0.; 1.]] |}]
  ;;
