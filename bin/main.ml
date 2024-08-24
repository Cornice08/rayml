open Rayml

let () = 
  let light = Light.point (Point.make (-10.) 10. (-10.)) Color.white in 
  let world = World.make light in
  let material = Material.default () in 
  let material = {material with 
    color = Color.make 1.0 0.9 0.9; 
    specular = 0.0; 
    ambient = 0.2}
  in
  let floor = Plane.make () in 
  let floor = {floor with material = material }
  in
  let transform = Matrix.sequence [
    Matrix.translation 0. 0. 5.;
    Matrix.rotation_y ((-.Float.pi) /. 4.);
    Matrix.rotation_x (Float.pi /. 2.);
    ]
  in 
  let left_wall = Plane.{inverse_transform = transform |> Matrix.inverse; material} in
  let transform = Matrix.sequence [
    Matrix.translation 0. 0. 5.;
    Matrix.rotation_y ((Float.pi) /. 4.);
    Matrix.rotation_x (Float.pi /. 2.);
    ]
  in 
  let right_wall = Plane.{inverse_transform = transform |> Matrix.inverse; material} in

  let material = {material with 
    color = Color.make 0.1 1. 0.5;
    diffuse = 0.7;
    specular = 0.3;
  }
  in
  let middle = Sphere.unit () in 
  let middle = { middle with material; inverse_transform = Matrix.sequence [
    Matrix.translation (-0.5) 1. 0.5
  ] |> Matrix.inverse}
  in
  let material = {material with 
    color = Color.make 0.5 1. 0.1;
  }
  in
  let right = Sphere.unit () in 
  let right = { right with material; inverse_transform = Matrix.sequence [
    Matrix.translation 1.5 0.5 (-0.5);
    Matrix.scaling 0.5
  ]|> Matrix.inverse}
  in
  let material = {material with 
    color = Color.make 1. 0.8 0.1;
  }
  in
  let left = Sphere.unit () in 
  let left = { left with material; inverse_transform = Matrix.sequence [
    Matrix.translation (-1.5) 0.33 (-0.75);
    Matrix.scaling 0.33
  ] |> Matrix.inverse}
  in

  let world = World.add_object world (Plane floor) in 
  let world = World.add_object world (Plane left_wall) in
  let world = World.add_object world (Plane right_wall) in
  let world = World.add_object world (Sphere middle) in 
  let world = World.add_object world (Sphere left) in 
  let world = World.add_object world (Sphere right) in 

  let camera = Camera.make 1000 500 (Float.pi /. 3.) in 
  let from = Point.make 0. 1.5 (-5.) in 
  let to_ = Point.make 0. 1. 0. in 
  let up = Point.make 0. 1. 0. in 
  let camera = { camera with inverse_transform = Matrix.view_transform from to_ up |> Matrix.inverse} in 

  let canvas = Camera.render camera world in 
  Canvas.print canvas
