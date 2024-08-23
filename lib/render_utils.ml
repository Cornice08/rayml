open! Core

let color_with_light (hit: Shape.hit_record) (light: Light.t) is_shadowed : Color.t = 
  let point = hit.point in 
  let normal = hit.normal in 
  let material = hit.material in 
  let material_color = material.color in 
  
  let effective_color = Vector.mul material_color light.intensity in 

  let light_vector = Vector.sub light.position point |> Vector.normalize in 

  let ambient = Vector.scalar_mul effective_color material.ambient in 

  let calculate_color () = 
    let light_dot_normal = Vector.dot light_vector normal in 
    let diffuse, specular = if Float.(light_dot_normal < 0.) then 
      Color.black, Color.black 
    else 
      let diffuse = Vector.scalar_mul effective_color (material.diffuse *. light_dot_normal) in 
      let reflect_v = Vector.neg light_vector |> Vector.reflect_about_normal ~normal in 

      let reflect_dot_eye = Vector.dot reflect_v hit.eye in 
      let specular = if Float.(reflect_dot_eye <= 0.) then 
        Color.black
      else
        let factor = reflect_dot_eye ** material.shininess in 
        Vector.scalar_mul light.intensity (material.specular *. factor)
      in 
      diffuse, specular
    in
    Vector.add ambient diffuse |> Vector.add specular
  in
  if is_shadowed then 
    ambient 
  else 
    calculate_color () 

