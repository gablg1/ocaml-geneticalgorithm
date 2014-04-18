open Core.Std
open Graphics
open Polygon

module type GUESS =
sig
  type guess

  (* Returns an image of the guess *)
  val image_of_guess : guess -> image
  
  (* Measures the fitness of the guess against the target.
   * The greater the weight (between 0 and MAX_COST) the better. *)
  val cost : guess -> image -> float

  (* Performs sexual reproduction between this guess and another guess
   * by calling reproduction on each of the correspondent polygons.
   * Returns a daughter guess *)
  val sexual_reproduction : guess -> guess -> guess
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module Guess : GUESS with type guess=polygon list =
struct

  type guess = polygon list

  let image_of_guest g = 

  let cost g i = 

  let map2 f xs ys =
    match xs,ys with
    | [], [] -> []
    | x::xs',y::ys' -> (f x y)::(map2 f xs' ys')
    | _ -> failwith "Invalid input - lists must be of equal length"

  let sexual_reproduction g1 g2 = map2 Polygon.sexual_reproduction g1 g2

  let run_tests () = 

end
