(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
 *)



let b_to_int str =
  let rec binary_to_int str x e count =
    match e with
    | e when e < 0 -> count
    | _ -> if (str.[x] == '1') then count + (power 2 e) + (binary_to_int str (x+1) (e-1) count)
           else
             (binary_to_int str (x+1) (e-1) count)
  in
  binary_to_int str 0 (String.length(str)-1) 0;;


let int_to_b x =
  let rec binary x str =
    match x with
        x when x = 0 -> str
      | _ -> if x mod 2 = 0 then
          (binary (x/2) str)^"0"^str
        else
          (binary (x/2) str)^"1"^str
  in
  binary x "";;


let encode str bits =
  let rec rec_encode x string =
    match x with
      x when x = (String.length(str)) -> string
    | _ -> (int_to_b (int_of_char(str.[x])))^string^(rec_encode (x+1) string)
  in
  b_to_int(rec_encode 0 "");;




(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ;
           alphanumeric ASCII is 7.
*)



 let rec shortcut_b string strr bits count x =
  match count with
      count when count mod bits = 0 && count = x -> strr
    | _ -> if string.[count] == '1' then strr^"1"^(shortcut_b string strr bits (count+1) x)
      else
        strr^"0"^(shortcut_b string strr bits (count+1) x);;

let decode msg bits =
  let txt = int_to_b(msg) in
  let x = String.length (txt) in
  let rec rec_decode count str y =
    match count with
       count when count = x -> str
      | _ -> str ^(Char.escaped(char_of_int(b_to_int(shortcut_b txt "" bits count y))))^(rec_decode (count+7) str (y+7))
  in
  rec_decode 0 "" bits;;

