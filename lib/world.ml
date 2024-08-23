open! Core 

type t = {
  light: Light.t; 
  objects: Shape.t list;
}

let make light = {
  light; 
  objects = []
}

let add_object world obj = 
  { world with objects = obj :: world.objects}

let intersect world ray = 
  List.map ~f:(fun obj -> Shape.intersect obj ray) world.objects |> List.concat

let is_shadowed world point = 
  let v = Vector.sub world.light.position point in 
  let distance = Vector.magnitude v in 
  let direction = Vector.normalize v in 

  let ray = Ray.make point direction in 

  let hit = Shape.hit (intersect world ray) ray in 
  match hit with 
  | Some hit when Float.(hit.t < distance) -> true 
  | _ -> false

let color_at world ray = 
  let intersections = intersect world ray in 
  let hit = Shape.hit intersections ray in 
  match hit with 
  | Some hit -> 
    let shadow = is_shadowed world hit.over_point in 
    Render_utils.color_with_light hit world.light shadow
  | None -> Color.black


