type t = {
  color: Color.t; 
  ambient: float; 
  diffuse: float;
  specular: float;
  shininess: float; 
}

let make color ambient diffuse specular shininess = 
  {
    color;
    ambient;
    diffuse;
    specular;
    shininess;
  }

let default () = 
  make Color.white 0.1 0.9 0.9 200.