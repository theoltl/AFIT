(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n =
  let rec recprime d =
    match d with
      d when n = d -> true
    | d when n mod d = 0 -> false
    | _ -> recprime (d+1)
  in
  if n < 2 then false
  else
    recprime 2;;

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
let is_pseudo_prime p test_seq =
  let rec recpseudo p seq =
    match seq with
      [] -> true
     |e::l when mod_power e (p-1) p = 1 -> recpseudo p l
     |_ -> false
  in
  if p < 2 then false
  else
    if p = 2 then true
    else
      recpseudo p test_seq;;
