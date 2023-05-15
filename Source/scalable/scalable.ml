(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
context zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code. A natural bitarray is understood as being a bitarray of
which you've taken out the sign bit, it is just the binary
decomposition of a non-negative integer.

 *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
*)
let from_int x =
  let rec rec_int x list =
    match x with
        x when x = 0 -> list
      | _ -> if x mod 2 = 0 then
          0::(rec_int (x/2) list)
        else
	  1::(rec_int (x/2) list)
  in
  if x < 0 then 1::rec_int (-x) []
  else
    if x = 0 then [0;0]
    else
      0::rec_int x [];;

	

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
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



let to_int bA =
  let rec rec_toint list exp count =
    match list with
        [] -> count
    | e::l -> if e = 1 then count + (power 2 exp) + (rec_toint l (exp+1) count)
      else (rec_toint l (exp+1) count)
  in
  match bA with
      [] -> 0
    |e::l -> if e = 1 then 
	  (rec_toint l 0 0)*(-1)
	else
	  (rec_toint l 0 0);;


(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
*)

let print_b bA =
  let rec recprint list =
    match list with
      | [] -> ()
      | e::l -> print_int e; recprint l
  in
  match bA with
       [] -> print_int(0)
    |e::l -> print_int(e); recprint(List.rev(l));;


(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(*#install_printer print_b*)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 if it is smaller and 0 in case of equality.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
*)
let rec compare_n nA nB =
      let rec rec_compare nA nB = 
	match (nA,nB) with
	    ([],[]) -> 0
	  |(_,[]) -> 1
	  |([],_) -> (-1)
	  |(e::l, f::q) ->
            if e<f then -1
            else
	      if e>f then
		1
              else
		rec_compare l q
      in
      match (nA,nB) with
	  ([],[]) -> 0
	|(_,[]) -> 1
	|([],_) -> (-1)
	| (e::l,f::q) -> rec_compare (List.rev(l)) (List.rev(q));;



(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (>>!) nA nB =
  if compare nA nB = 1 then true
  else
    false;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<<!) nA nB =
  if compare nA nB = (-1) then true
  else
    false;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (>=!) nA nB =
  if (compare nA nB = 0) || (compare nA nB = 1) then true
  else
    false;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<=!) nA nB =
  if (compare nA nB = 0) || (compare nA nB = (-1)) then true
  else
    false;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 if it smaller and 0 in case of equality.
    @param bA A bitarray.
    @param bB A bitarray.
*)

let compare_b bA bB =
  let rec rec_compare bA bB = 
    match (bA,bB) with
	([],[]) -> 0
      |(_,[]) -> 1
      |([],_) -> (-1)
      |(e::l, f::q) ->
        if e<f then -1
        else
	  if e>f then
	    1
          else
	    compare_n l q
  in
 match (bA,bB) with
      ([],[]) -> 0
    |(_,[]) -> 1
    |([],_) -> (-1)
    | (e::l,f::q) ->
      if e<f then -1
      else
	if e>f then
	  1
        else
	  rec_compare (List.rev(l)) (List.rev(q));;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
*)
let (<<) nA nB =
  if compare_n nA nB = 1 then
    true
  else
    false;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
   if compare_b bA bB = (-1) then
    true
  else
    false;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) nA nB =
   if (compare_n nA nB = 0) || (compare nA nB = 1) then true
  else
    false;;


(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) nA nB =
  if (compare_n nA nB = 0) || (compare nA nB = (-1)) then true
  else
    false;;

(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  match bA with
      []-> 0
    |e::l-> if e = 0
      then 1
      else
	(-1);;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  match bA with
      []-> []
    |e::l-> if e = 1
      then 0::l
      else
        bA;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = 0

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (0, 0)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  let nA = abs_b (nA) and nB = abs_b (nB) in
  from_int ((to_int nA)+(to_int nB));;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  let nA = abs_b (nA) and nB = abs_b (nB) in
    from_int ((to_int nA)-(to_int nB));;
  

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b bA bB =
    from_int ((to_int bA)+(to_int bB));;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let diff_b bA bB =
    from_int ((to_int bA)-(to_int bB));;

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = []

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
*)
let mult_b bA bB =
    from_int ((to_int bA)*(to_int bB));;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
*)

let quot_b bA bB =
  if (to_int bB <= 0) then invalid_arg "b should be positive"
  else
    if ((to_int bA) mod (to_int bB) < 0) then from_int ((to_int bA) / (to_int bB) -1)
    else
      from_int ((to_int bA) / (to_int bB));;

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
*)
let mod_b bA bB =
  if (to_int bA < 0 && to_int bB < 0) then  from_int ((to_int bA) mod  (to_int bB))
  else
    if (to_int bA mod to_int bB) < 0 then from_int ((to_int bA mod to_int bB) + to_int bB)
    else
      from_int ((to_int bA) mod (to_int bB));;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB =
  (quot_b bA bB, mod_b bA bB);;
