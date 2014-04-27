open Core.Std
open Graphics
open Helpers.Helpers
open Polygon

module type GUESS =
sig
  type polygon

  type guess

  (* fresh m v returns a new random guess with m Ps and v vertices *)
  val fresh : int -> int -> guess

  (* Returns an image of the guess *)
  val image_of_guess : guess -> image
  
  (* Measures the fitness of the guess against the target.
   * The greater the weight (between 0 and MAX_COST) the better. *)
  val cost : guess -> image -> float

  (* Makes a new guess out of a polygon list *)
  val make : polygon list -> guess

  (* Performs sexual reproduction between this guess and another guess
   * by calling reproduction on each of the correspondent Ps.
   * Third argument is the level of chaos of the reproduction
   * Returns a daughter guess *)
  val sexual_reproduction : float -> guess -> guess -> guess

  (* Returns true if two guesses are equal and false otherwise *)
  val equals : guess -> guess -> bool

  (* Prints a Guess *)
  val print : guess -> unit
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module MakeGuess (P : POLYGON) : GUESS with type guess=P.polygon array =
struct
  type polygon = P.polygon
  type guess = polygon array
  
  let rec fresh m v = failwith "TODO"
    
  
  (* Makes a guess out of a list of polygons *)
  let make lst = Array.of_list lst

  let image_of_guess _ = Graphics.make_image (Array.make_matrix ~dimx:5 ~dimy:12 black)

  let cost _ _ = failwith "TODO"

  let sexual_reproduction std_dev g1 g2 = Array.map2_exn ~f:(P.sexual_reproduction std_dev) g1 g2
  
  let equals g1 g2 = Array.fold2_exn ~f:(fun r p1 p2 -> (P.equals p1 p2) && r) ~init:(true) g1 g2
  
  let print g =
    print_endline "################ Start of Guess ################";
    Array.iter ~f:(P.print) g;
    print_endline "################ End of Guess ################";
    print_endline ""
  
  let test_sexual_reproduction () =
    let p1 = P.make [(0.,0.);(1.,1.);(2.,2.);(3.,3.)] blue in
    let p2 = P.make [(1.,2.);(3.,4.);(5.,6.);(7.,8.)] red in
    let p3 = P.make [(5.,12.);(4.,9.);(9.,1.);(9.,3.)] green in
    let g1 = make [p1] in
    let g2 = make [p2] in
    let g3 = make [p3] in
    let g4 = make [p1; p2] in
    let g5 = make [p2; p3] in
    let g6 = make [p3; p1] in
    
    (* Answers *)
    let r1 = P.make [(0.5,1.);(2.,2.5);(3.5,4.);(5.,5.5)] (halfway_color blue red) in
    let r2 = P.make [(3.,7.);(3.5,6.5);(7.,3.5);(8.,5.5)] (halfway_color green red) in
    let r3 = P.make [(2.5,6.);(2.5,5.);(5.5,1.5);(6.,3.)] (halfway_color blue green) in
    
    assert(equals (sexual_reproduction 0. g1 g2) (make [r1]));
    assert(equals (sexual_reproduction 0. g2 g3) (make [r2]));
    assert(equals (sexual_reproduction 0. g1 g3) (make [r3]));
    assert(equals (sexual_reproduction 0. g4 g5) (make [r1; r2]));
    assert(equals (sexual_reproduction 0. g4 g6) (make [r3; r1]));
    assert(equals (sexual_reproduction 0. g5 g6) (make [r2; r3]));
    
    let d = sexual_reproduction 1. g4 g6 in
    print d

  let run_tests () =
    test_sexual_reproduction ();
    () 
end

(* Applies the functor to make a Guess using our implementation of Polygon *)
module Guess = MakeGuess(Polygon)
