open Core

type t = {
    width: int; 
    height: int; 
    pixels: Color.t array array;
}

let make width height = 
  let pixels = Array.make_matrix ~dimx:width ~dimy:height Color.white in
  {
    width;
    height;
    pixels;
  }

let pixel_at canvas x y = 
  canvas.pixels.(x).(y)

let set_pixel_at canvas x y color = 
  canvas.pixels.(x).(y) <- color

let print canvas = 
  Printf.printf "P3\n%d %d\n255\n" canvas.width canvas.height;
  for j = 0 to canvas.height-1 do 
    for i = 0 to canvas.width-1 do 
      let color = pixel_at canvas i j in
      let str = Color.to_ppm color in 
      Printf.printf "%s\n" str;
    done;
  done;
  


