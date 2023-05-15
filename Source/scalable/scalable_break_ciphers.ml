(** Factoring bitarrays into primes *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_generate_primes

(** Factors product of two prime bitarrays.
    @param key is public key of an RSA cryptosystem.
 *)

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

let eratosthenes n =
  let rec prime_era n d l =
    match n with
      n when d == n -> if is_prime n = true then
                         n::l
                       else
                         l
    | n when is_prime d == true -> d::prime_era n (d+1) l
    | _ -> prime_era n (d+1) l
  in
  if n < 2 then invalid_arg "n must be higher then 1"
  else
    if n = 2 then [2]
    else
      if n mod 2 = 0 then prime_era (n-1) 2 []
      else
        prime_era n 2 [];;


let break key =
  let (x,_) = key in
  let rec rec_break liste =
    match liste with
      [] -> ([0;0],[0;0])
     |e::l when mod_b x (from_int e) = [0;0] -> ((from_int e), quot_b x (from_int e))
     |e::l -> rec_break l
          
  in
  rec_break (eratosthenes 10000);;
