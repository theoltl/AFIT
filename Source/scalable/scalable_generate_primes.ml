(** Generating prime bitarrays *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_test_primes

(* Initializing list of bitarrays for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
 let rec rec_era n d =
    match n with
	n when d = (from_int n) -> [d]
      | n when d = (from_int 2) -> d::rec_era n (add_b d [0;1])
      | _ ->  d::rec_era n (add_b d (from_int 2))
  in
  if n < 2 then invalid_arg "n must be higher then 1"
  else
    if n = 2 then [[0; 0; 1]]
    else
      if  n mod 2 = 0 then rec_era (n-1) [0; 0; 1]
      else
        rec_era n [0; 0; 1];;
      
  
(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  let rec prime_era n d l =
    match n with
      n when d == n -> if is_prime (from_int n) = true then
                         (from_int n)::l
                       else
                         l
      | n when is_prime (from_int d) == true -> (from_int d)::prime_era n (d+1) l
      | _ -> prime_era n (d+1) l
  in
   if n < 2 then invalid_arg "n must be higher then 1"
   else
     if n = 2 then [(from_int 2)]
     else
       if n mod 2 = 0 then prime_era (n-1) 2 []
       else
         prime_era n 2 [];;


(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file = ()

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c = ()

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime bitarrays for specific or fun
   purposes.
 *)

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)

let double_primes limit isprime =
    let rec rec_double_primes d =
    match d with
      d when d = limit -> []
      | d when isprime (from_int d) && isprime (from_int(d*2+1)) -> (from_int d,from_int (d*2+1))::rec_double_primes
	(d+1)
    | _ -> rec_double_primes (d+1)
  in
  rec_double_primes 2;;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec rec_twin_primes d =
    match d with
	d when d = limit -> []
      | d when isprime (from_int d) && isprime (from_int (d+2)) -> (from_int d, from_int (d+2))::rec_twin_primes (d+1)
      | _ -> rec_twin_primes (d+1)
  in
  rec_twin_primes 2;;
