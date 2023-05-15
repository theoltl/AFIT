(** Generating primes *)

open Builtin
open Basic_arithmetics
open Test_primes

(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)
let init_eratosthenes n =
  let rec rec_era n d l =
    match n with
	n when d == n -> n::l
      | n when d == 2 -> d::rec_era n (d+1) l
      | _ ->  d::rec_era n (d+2) l
  in
  if n < 2 then invalid_arg "n must be higher then 1"
  else
    if n = 2 then [2]
    else
      if n mod 2 = 0 then rec_era (n-1) 2 []
      else
        rec_era n 2 [];;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
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
		

(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  open_in file;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c = ()

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: Your list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec rec_double_primes d =
    match d with
      d when d == limit -> []
    | d when isprime d && isprime (d*2+1) -> (d, d*2+1)::rec_double_primes (d+1)
    | _ -> rec_double_primes (d+1)
  in
  rec_double_primes 2;;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec rec_twin_primes d =
    match d with
      d when d == limit -> []
    | d when isprime d && isprime (d+2) -> (d, d+2)::rec_twin_primes (d+1)
    | _ -> rec_twin_primes (d+1)
  in
  rec_twin_primes 2;;
                                  
