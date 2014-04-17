open Core.Std
open Graphics

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