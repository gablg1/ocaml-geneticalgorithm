open Core.Std
open Graphics
open Polygon
open Helpers.Helpers

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
  
  (* Makes a guess out of a list of polygons *)
  let make_guess lst = lst

  let image_of_guess _ = Graphics.make_image (Array.make_matrix ~dimx:5 ~dimy:12 black)

  let cost _ _ = 0.0

  let sexual_reproduction std_dev g1 g2 = List.map2_exn g1 g2 ~f:(Polygon.sexual_reproduction std_dev)
  
  let test_sexual_reproduction () =
    let p1 = Polygon.make_polygon [(0,0);(1,1);(2,2);(3,3)] blue in
    let p2 = Polygon.make_polygon [(1,2);(3,4);(5,6);(7,8)] red in
    let p3 = Polygon.make_polygon [(5,12);(4,9);(9,1);(9,3)] green in
    let g1 = make_guess [p1] in
    let g2 = make_guess [p2] in
    let g3 = make_guess [p3] in
    let g4 = make_guess [p1; p2] in
    let g5 = make_guess [p2; p3] in
    let g6 = make_guess [p3; p1] in
    
    let daughter = sexual_reproduction 0. g1 g2 in
    let tuple = match daughter with
    | [a] -> a
    | _ -> failwith "bug1" in
    print_endline (string_of_int (snd tuple));
    print_endline (string_of_int ((blue+red)/2))
    (*
    assert(sexual_reproduction 0. g1 g2 = [([(0,1);(2,2);(3,4);(5,5)], halfway_color blue red)]);
    assert(sexual_reproduction 0. g2 g3 = [([(3,7);(3,6);(7,3);(8,5)], halfway_color green red)]);
    assert(sexual_reproduction 0. g1 g3 = [([(2,6);(2,5);(5,1);(6,3)], halfway_color blue green)]);
    assert(sexual_reproduction 0. g4 g5 = 
      [([(0,1);(2,2);(3,4);(5,5)], halfway_color blue red); ([(3,7);(3,6);(7,3);(8,5)], halfway_color red green)]);
    assert(sexual_reproduction 0. g5 g6 = 
      [([(3,7);(3,6);(7,3);(8,5)], halfway_color green red); ([(2,6);(2,5);(5,1);(6,3)], halfway_color blue green)]);
    assert(sexual_reproduction 0. g4 g6 = 
      [([(2,6);(2,5);(5,1);(6,3)], halfway_color blue green); ([(0,1);(2,2);(3,4);(5,5)], halfway_color blue red)])
      *)

  let run_tests () =
    test_sexual_reproduction ();
    ()
    
end
