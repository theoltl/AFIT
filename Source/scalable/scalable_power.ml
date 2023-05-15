(** Power function implementations for bitarrays *)

open Scalable
open Scalable_basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class in the
   built-in integer case.
*)

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n =
  if to_int n < 0 then invalid_arg "n should be positive"
  else
    let rec poww n =
      match n with
          [0; 0] -> (from_int 1)
	| _ -> mult_b x (poww (from_int((to_int n) - 1)))
    in poww n;;


(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)

let power x n =
  if to_int n < 0 then invalid_arg "n should be positive"
  else
    let rec powerr n =
      match n with
           [0; 0] -> from_int 1
        | [0; 1] ->  mult_b x (powerr (from_int((to_int n) - 1)))
        | _ -> mult_b (mult_b x x) (powerr (from_int((to_int n) - 2)))
    in powerr n;;
  
(* Modular expnonentiation ; modulo a given natural (bitarray without
   sign bits).
*)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)

let mod_power x n m =
  let rec rec_mod_power x n m =
    match n with
      |  [0; 0] -> from_int 1
      | _ -> if (to_int n) mod 2 = 0 then
          let temp = rec_mod_power x (quot_b n (from_int 2)) m
	  in mod_b (mult_b temp temp) m
        else
          (mod_b (mult_b (mod_b x m) (rec_mod_power x (from_int((to_int n) - 1)) m)) m)
  in
  rec_mod_power x n m;;


(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p = 
  if (to_int x) = 0 then (from_int 0)
  else
    if (to_int n) = 0 then (from_int 1)
    else
      let rec rec_prime_mod_power x n p =
        if (to_int n) >= ((to_int p)-1) then
          mod_power x (mod_b n (from_int((to_int(p))-1))) p
        else
          mod_power x n p
      in rec_prime_mod_power x n p;;
