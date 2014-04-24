open Core.Std
open Graphics
open Polygon

module type GUESS =
sig
  type guess

  (* Returns a fresh new random guess *)
  val fresh : int -> guess

  (* Returns an image of the guess *)
  val image_of_guess : guess -> image
  
  (* Measures the fitness of the guess against the target.
   * The greater the weight (between 0 and MAX_COST) the better. *)
  val cost : guess -> image -> float

  (* Performs sexual reproduction between this guess and another guess
   * by calling reproduction on each of the correspondent polygons.
   * Third argument is the level of chaos of the reproduction
   * Returns a daughter guess *)
  val sexual_reproduction : float -> guess -> guess -> guess
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module Guess : GUESS with type guess=Polygon.polygon list =
struct

  type guess = Polygon.polygon list

  let fresh _ = []

  let image_of_guess _ = Graphics.make_image (Array.make_matrix ~dimx:5 ~dimy:12 black)

  let cost _ _ = 0.0

  let sexual_reproduction std_dev g1 g2 = List.map2_exn g1 g2 ~f:(Polygon.sexual_reproduction std_dev)

  let test_sexual_reproduction () =
    let p1 = ([(0,0);(1,1);(2,2);(3,3)], blue) in
    let p2 = ([(1,2);(3,4);(5,6);(7,8)], red) in
    let p3 = ([(5,12);(4,9);(9,1);(9,3)], green) in
    let g1 = [p1] in
    let g2 = [p2] in
    let g3 = [p3] in
    let g4 = [p1; p2] in
    let g5 = [p2; p3] in
    let g6 = [p3; p1] in
    assert(sexual_reproduction 0. g1 g2 = [([(0,1);(2,2);(3,4);(5,5)],(blue+red)/2)]);
    assert(sexual_reproduction 0. g2 g3 = [([(3,7);(3,6);(7,3);(8,5)],(red+green)/2)]);
    assert(sexual_reproduction 0. g1 g3 = [([(2,6);(2,5);(5,1);(6,3)],(blue+green)/2)]);
    assert(sexual_reproduction 0. g4 g5 = [([(0,1);(2,2);(3,4);(5,5)],(blue+red)/2); ([(3,7);(3,6);(7,3);(8,5)],(red+green)/2)]);
    assert(sexual_reproduction 0. g5 g6 = [([(3,7);(3,6);(7,3);(8,5)],(red+green)/2); ([(2,6);(2,5);(5,1);(6,3)],(blue+green)/2)]);
    assert(sexual_reproduction 0. g4 g6 = [([(2,6);(2,5);(5,1);(6,3)],(blue+green)/2); ([(0,1);(2,2);(3,4);(5,5)],(blue+red)/2)])

  let run_tests () =
    test_sexual_reproduction ();
    ()
    
end
