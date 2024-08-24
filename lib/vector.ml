open! Core

type t = float list [@@deriving show]

let of_list l = l

let make x y z = [x; y; z]

let scalar_mul v x = 
  List.map ~f:(fun e -> e *. x) v

let scalar_div v x = 
  List.map ~f:(fun e -> e /. x) v

let sqr v = 
  List.map ~f:(fun e -> e ** 2.) v

let sum v = 
  List.fold_left ~f:(+.) ~init:0. v 

let magnitude v = 
  let sqrd = sqr v in 
  let sum = sum sqrd in 
  Float.sqrt sum

let normalize v : t = 
  let mag = magnitude v in
  List.map ~f:(fun e -> e /. mag) v

let mul v1 v2 = 
  List.map2_exn ~f:( *.) v1 v2

let sub v1 v2 : t = 
  List.map2_exn ~f:(-.) v1 v2

let add v1 v2 : t = 
  List.map2_exn ~f:(+.) v1 v2

let neg v = 
  List.map ~f:(fun a -> -.a) v

let dot a b = 
  sum (mul a b)

let elements v = 
  match v with 
  | [x; y; z] -> (x, y, z)
  | _ -> failwith "Vector should contain three elements" 

let cross a b : t = 
 let (ax, ay, az) = elements a in 
 let (bx, by, bz) = elements b in 

[
  ay *. bz -. az *. by; 
  az *. bx -. ax *. bz; 
  ax *. by -. ay *. bx
]

let reflect_about_normal v ~normal = 
  sub v (scalar_mul normal (2. *. dot v normal))


let%expect_test "reflect 45 degree vector" = 
  let v = make 1. (-1.) 0. in 
  let normal = make 0. 1. 0. in 

  let reflect = reflect_about_normal v ~normal in 

  Format.printf "%a" pp reflect;

  [%expect {| [1.; 1.; 0.] |}]
;;