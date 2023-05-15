(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics
open Generate_primes
(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
*)

let break key =
  let (x,_) = key in
  let rec rec_break liste =
    match liste with
	[] -> (0,0)
      |e::l when x mod e = 0 -> (e, x/e)
      |e::l -> rec_break l
        
  in
  rec_break (eratosthenes (15000));;
