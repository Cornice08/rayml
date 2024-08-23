open Lacaml.D
open Core 

module T = Domainslib.Task

type t = {
  hsize: int;
  vsize: int;
  transform: mat;
  pixel_size: float;
  half_width: float; 
  half_height: float;
}

let make hsize vsize field_of_view = 
  let half_view = Float.tan(field_of_view /. 2.) in 
  let aspect = (Float.of_int hsize) /. (Float.of_int vsize) in 

  let half_width, half_height = if Float.(aspect >= 1.) then 
    half_view, half_view /. aspect else 
    half_view *. aspect, half_view
  in 

  let pixel_size = (half_width *. 2.) /. (Float.of_int hsize) in 

  {
    hsize; 
    vsize; 
    pixel_size; 
    half_width; 
    half_height; 
    transform = Mat.identity 4;
  }

let set_transform camera transform = 
  { camera with transform = transform }

let ray_for_pixel camera x y : Ray.t = 
  let xoffset = ((Float.of_int x) +. 0.5) *. camera.pixel_size in 
  let yoffset = ((Float.of_int y) +. 0.5) *. camera.pixel_size in 

  let world_x = camera.half_width -. xoffset in 
  let world_y = camera.half_height -. yoffset in 

  let inverse = Matrix.inverse camera.transform in

  let pixel = Matrix.transform_point inverse (Point.make world_x world_y (-1.)) in 

  let origin = Matrix.transform_point inverse Point.origin in 

  let direction = Vector.normalize (Vec.sub pixel origin) in 
  Ray.make origin direction

let render camera world : Canvas.t = 
  let hsize, vsize = camera.hsize, camera.vsize in 
  let canvas = Canvas.make hsize vsize in 
  let pool = T.setup_pool ~num_domains:8 () in 
  T.run pool (fun _ -> 
    T.parallel_for pool ~start:0 ~finish:(canvas.height-1) ~body:(fun j ->
      for i = 0 to canvas.width-1 do 
      (* Printf.eprintf "Scanlines remaining %d\n %!" (vsize - j); *)
        let ray = ray_for_pixel camera i j in 
        let color = World.color_at world ray in 
        Canvas.set_pixel_at canvas i j color;
      done
      )
  );
  T.teardown_pool pool;

  canvas

let%expect_test "pixel size for horizontal" = 
  let camera = make 200 125 (Float.pi /. 2.) in 

  Format.printf "%f" camera.pixel_size;

  [%expect {| 0.010000 |}];
;;

let%expect_test "pixel size for vertical" = 
  let camera = make 125 200 (Float.pi /. 2.) in 

  Format.printf "%f" camera.pixel_size;

  [%expect {| 0.010000 |}];
;;

let%expect_test "ray through corner of canvas" = 
  let camera = make 201 101 (Float.pi /. 2.) in 
  let ray = ray_for_pixel camera 0 0 in 

  Format.printf "%a" pp_vec ray.origin;
  [%expect {|
    0
    0
    0
    |}];
    Format.printf "%a" pp_vec ray.direction;
  [%expect {|
     0.665186
     0.332593
    -0.668512
    |}];
;;
