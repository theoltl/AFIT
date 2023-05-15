(** Power function implementations for built-in integers *)


open Builtin
open Basic_arithmetics

   
(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let pow x n =
  if n < 0 then invalid_arg "n should be positive"
  else
    let rec poww n =
    match n with
        0 -> 1
      | _ -> x*poww (n-1)
  in poww n;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
   if n < 0 then invalid_arg "n should be positive"
   else
     let rec powerr n =
       match n with
           0 -> 1
         | 1 -> x*powerr (n-1)
         | _ -> x*x*powerr (n-2)
     in powerr n;;

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
*)


let mod_power x n m =
  let rec rec_mod_power x n m =
    match n with
      | 0 -> 1
      | _ -> if n mod 2 = 0 then
          let temp = rec_mod_power x (n/2) m in modulo (temp*temp) m
        else
          (modulo ((modulo x m) * (rec_mod_power x (n-1) m)) m)
  in
  rec_mod_power x n m;;

(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  if x = 0 then 0
  else
    if n = 0 then 1
    else
      let rec rec_prime_mod_power x n p =
        if n >= (p-1) then
          mod_power x (modulo n (p-1)) p
        else
          mod_power x n p
      in rec_prime_mod_power x n p;;
