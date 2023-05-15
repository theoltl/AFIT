(** Basic arithmetics for ordered euclidian ring. *)

open Scalable

(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)

let gcd_b bA bB =
  let rec recgcd_b bA bB = 
    if to_int bA = 0 then bB
    else
      abs_b((recgcd_b (from_int((to_int bB) mod (to_int bA))) bA))
  in
  recgcd_b bA bB;;


(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)

let bezout_b bA bB =
  let rec rec_bezout(u0,v0,r0,u1,v1,r1) =
    if r1 = from_int 0 then (u0,v0,r0)
    else
      let diviseur = quot_b r0 r1
      in
      rec_bezout(u1,v1,r1,diff_b u0 (mult_b diviseur u1),diff_b v0 (mult_b diviseur v1),diff_b r0 (mult_b diviseur r1))
  in
  rec_bezout((from_int 1),(from_int 0),bA,(from_int 0),(from_int 1),bB);;



