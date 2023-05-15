(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
   
let is_prime n =
  let rec recprime d =
    match d with
      d when n = d -> true
    | d when mod_b n d = [0;0] -> false
    | _ -> recprime (add_b d [0;1])
  in
  if to_int n < 2 then false
  else
    recprime (from_int 2);;

  
(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec recpseudo p seq =
    match seq with
      [] -> true
     |e::l when mod_power e (diff_n p [0;1]) p = [0;1] -> recpseudo p l
     |_ -> false
  in
  if (to_int p) < 2 then false
  else
    if (to_int p) = 2 then true
    else
      recpseudo p test_seq;;
