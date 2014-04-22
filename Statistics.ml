open Core.Std
open Graphics
open Guess
open Random

module type STATISTICS =
sig
  type mean 

  type std_dev 
  
  (* Returns a pseudorandom float between 0 and 1 *)
  val box_muller : () -> float

  (* Returns a pseudorandom float in the gaussian distribution *)
  val gaussian_pick : mean -> std_dev -> float
end

module Statistics : STATISTICS =
struct
  type mean = float

  type std_dev = float

  let box_muller () : float =
    let _ = Random.self_init in
    let u = Random.float 2.0 -. 1.0 in
    let v = Random.float 2.0 -. 1.0 in
    let s = u *. u +. v *. v in
    if (s >= 1.0 || s = 0.0) then box_muller ()
    else u *. sqrt (-2.0 *. (log s) /. s)

  let gaussian_pick (m : mean) (s : std_dev) : float = 
    let n = box_muller () in
    n *. s +. m
    
end
