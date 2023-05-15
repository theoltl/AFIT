(** Ciphers
    bitarrays based ciphers.
*)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(********** RSA Cipher **********)

(** Generate an RSA ciphering key.
    Involved prime bitarrays need to be distinct. Output is a couple
    of public, private keys.
    @param p prime bitarray
    @param q prime bitarray
*)
let generate_keys_rsa p q =
  let n = (mult_b p q) in
  let phi = mult_b (diff_b p [0;1])(diff_b q [0;1])  in
  let rec public e =
    let (x,_,_) = bezout_b e phi in
    if gcd_b phi e = [0;1] then
      ((n, e),(n, x))
    else
      public (add_b e [0;1])
  in public (from_int(Random.int((to_int phi) - 1)));;
  
(** Encryption using RSA cryptosystem.
    @param m bitarray hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m bitarray hash of encrypted message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is a prime bitarray and g a bitarray having high enough order modulo p.
    @param p is a prime bitarray having form 2*q + 1 for prime bitarray q.
 *)
let rec public_data_g p =
  let g = from_int (Random.int ((to_int p) -1)+1) in
  if (mod_power g (from_int 2) p) != [0;1] then (g,p)
  else public_data_g p;;

(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
    let g2 = add_b [0;1] (from_int(Random.int ((to_int p)-1))) in
  ((mod_power g g2 p),g2);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let exp = from_int (Random.int 10000)
  in
  let times = (mod_power kA exp p)
  in
  (mod_power g exp p, mod_b (mult_b msg times) p);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
  let x = mod_power msgA a p in
  let (y,_,_) = bezout_b x p
  in
  mod_b (mult_b y  msgB) p;;
