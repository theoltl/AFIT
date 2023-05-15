open Builtin

let rec gcd a b =
    if a = 0 then b
    else
      if (a < 0 && b < 0)
      then ((-1)*(gcd (b mod a) a))
      else
        gcd (b mod a) a;;


let bezout a b =
  let rec rec_bezout(u0,v0,r0,u1,v1,r1) =
    if r1 = 0 then (u0,v0,r0)
    else
      let diviseur = quot r0 r1 in
      rec_bezout(u1,v1,r1,u0-diviseur*u1,v0-diviseur*v1,r0-diviseur*r1)
  in rec_bezout(1,0,a,0,1,b);;

