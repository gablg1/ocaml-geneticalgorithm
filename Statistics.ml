open Core.Std
open Random

module type STATISTICS =
sig
  type t 

  type std_dev 
  
  (* Returns a pseudorandom float between 0 and 1 *)
  val random : unit -> t

  (* Returns a pseudorandom t in the gaussian distribution *)
  val gaussian_pick : t -> std_dev -> t
end

(* Implements Statistics of Floats *)
module Statistics : (STATISTICS with type t = float
  with type std_dev = float) =
struct
  type t = float

  type std_dev = float

  (* Uses Box-Muller to implement pseudo-randomness *)
  let rec random () =
    let _ = Random.init (Unix.time ()) in
    let u = Random.float 2.0 -. 1.0 in
    let v = Random.float 2.0 -. 1.0 in
    let s = u *. u +. v *. v in
    if (s >= 1.0 || s = 0.0) then random ()
    else u *. sqrt (-2.0 *. (log s) /. s)

  let gaussian_pick m s = 
    let n = random () in
    n *. s +. m
    
end


