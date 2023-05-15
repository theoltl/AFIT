(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
 *)
let encrypt_cesar k m b =
  let rec encrypt_rec m2 =
    match m2 with
      [] -> []
    | e::l -> (modulo (e+k) b)::encrypt_rec l
  in
  encrypt_rec m;;


(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
 *)
let decrypt_cesar k m b =
  encrypt_cesar (-k) m b;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)

let generate_keys_rsa p q=
  let n= p*q in
  let phi= (p-1)*(q-1) in
  let rec public e=
    let (x,_,_) = bezout e phi in
    if gcd phi e = 1 then
      ((n, e),(n, x))
    else
      public (e+1)
  in public (Random.int(phi-1));;


(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let g = Random.int (p-1)+1 in
  if (mod_power g 2 p) != 1 then (g,p)
  else public_data_g p;;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let g2 = 1 + Random.int (p-1) in
  ((mod_power g g2 p),g2);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let exp = (Random.int 10000)
  in
  let times = (mod_power kA exp p)
  in
  (mod_power g exp p, modulo (msg*times) p);;


(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
 let decrypt_g (msgA, msgB) a (g, p) =
  let x = mod_power msgA a p in
  let (y,_,_) = bezout x p
  in
  modulo (y * msgB) p;;
