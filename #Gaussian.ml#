open Core.Std
open Graphics
open Guess
open Random

module type GAUSSIAN =
sig
  type gaussian

  (* Returns the mean of the gaussian distribution *)
  val mean_of_guess : gaussian -> float

  (* Returns the standard deviation of the gaussian distribution *)
  val std_dev_of_guess : gaussian -> float
  
  (* Returns a pseudorandom float between 0 and 1 *)
  val box_muller : () -> float

  (* Returns a pseudorandom float in the gaussian distribution *)
  val random_gaussian : gaussian -> float
end

module Gaussian : GAUSSIAN with type gaussian = Guess.guess list =
struct

  type gaussian = Guess.guess list

  let mean_of_guess (g : gaussian) : float = 
    (List.fold_right ~f:(fun x y -> x.cost +. y) ~init:0.0 g) /. (float (List.length g))

  let std_dev_of_guess (g : gaussian) : float = 
    let mean = mean_of_guess g in
    let variance = (List.fold_right ~f:(fun x y -> (x.cost -. mean) ** 2.0 +. y) ~init:0.0 g) 
      /. ((float (List.length g)) -. 1.0) in
    sqrt variance

  let box_muller () : float =
    let _ = Random.self_init in
    let u = Random.float 2.0 -. 1.0 in
    let v = Random.float 2.0 -. 1.0 in
    let s = u *. u +. v *. v in
    if (s >= 1.0 || s = 0.0) then box_muller ()
    else u *. sqrt (-2.0 *. (log s) /. s)

  let random_gaussian (g : gaussian) : float = 
    g.box_muller *. g.std_dev_of_guess +. g.mean_of_guess
    
end
