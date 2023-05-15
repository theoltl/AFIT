let sign x =
  if x < 0 then -1
  else 1;;


let quot a b =
  if (b <= 0) then invalid_arg "b should be positive"
  else
    if (a mod b < 0) then (a / b -1)
    else
      a / b;;



let modulo a b =
  if (a < 0 && b < 0) then a mod b
  else
    if (a mod b) < 0 then ((a mod b) + b)
    else a mod b;;


let div a b =
  (quot a b , modulo a b);;
