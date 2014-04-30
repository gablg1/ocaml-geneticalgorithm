open Core.Std
open Graphics
open Helpers.Helpers
open Polygon
open Circle
open Cost

module type GUESS =
sig
  (* lego is the building block of the image *)
  type lego

  type guess

  (* fresh w h m returns a new random guess with m legos 
   * where w and h are the width and height of the target image *)
  val fresh : int -> int -> int -> guess

  (* Returns list of legos that composes guess *)
  val legos : guess -> lego array

  (* Returns guess as color array array *)
  val matrix_of_guess : guess -> color array array
  
  (* Measures the fitness of the guess against the target.
   * The greater the weight (between 0 and MAX_COST) the better. *)
  val cost : guess -> image -> float

  (* Makes a new guess out of a polygon list *)
  val make : lego list -> guess

  (* Performs sexual reproduction between this guess and another guess
   * by calling reproduction on each of the correspondent legos.
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

module MakeCircleGuess (C : CIRCLE) : GUESS with type guess=C.circle array =
struct
  type lego = C.circle
  type guess = lego array
  
  let fresh width height m = Array.init ~f:(fun _ -> C.fresh width height) m
  
  let legos g = g

  let make lst = Array.of_list lst

  (* Placeholder *)
  let matrix_of_guess _ = Array.make_matrix ~dimx:5 ~dimy:12 black

  let cost _ _ = failwith "TODO"

  let sexual_reproduction std_dev g1 g2 = Array.map2_exn ~f:(C.sexual_reproduction std_dev) g1 g2
  
  let equals g1 g2 = Array.fold2_exn ~f:(fun r p1 p2 -> (C.equals p1 p2) && r) ~init:(true) g1 g2
  
  let print g =
    print_endline "################ Start of Guess ################";
    Array.iter ~f:(C.print) g;
    print_endline "################ End of Guess ################";
    print_endline ""
  
  let test_sexual_reproduction () =
    let p1 = C.make (0.,0.) 3. blue in
    let p2 = C.make (1.,2.) 4. red in
    let p3 = C.make (3.,4.) 5. green in
    
    let g1 = make [p1] in
    let g2 = make [p2] in
    let g3 = make [p3] in
    let g4 = make [p1; p2] in
    let g5 = make [p2; p3] in
    let g6 = make [p3; p1] in
    
    (* Answers *)
    let r1 = C.make (0.5,1.) 3.5 (halfway_color blue red) in
    let r2 = C.make (2.,3.) 4.5 (halfway_color red green) in
    let r3 = C.make (1.5,2.) 4. (halfway_color blue green) in
    
    assert(equals (sexual_reproduction 0. g1 g2) (make [r1]));
    assert(equals (sexual_reproduction 0. g2 g3) (make [r2]));
    assert(equals (sexual_reproduction 0. g1 g3) (make [r3]));
    assert(equals (sexual_reproduction 0. g4 g5) (make [r1; r2]));
    assert(equals (sexual_reproduction 0. g4 g6) (make [r3; r1]));
    assert(equals (sexual_reproduction 0. g5 g6) (make [r2; r3]))

  let run_tests () =
    test_sexual_reproduction ();
    () 
end

module MakePolygonGuess (P : POLYGON) : GUESS with type guess=P.polygon array =
struct
  type lego = P.polygon
  type guess = lego array
  
  let fresh_v width height m v = Array.init ~f:(fun _ -> P.fresh width height v) m
  
  (* Number of vertices is always 1 *)
  let fresh w h m = fresh_v w h m 1
  
  let legos g = g

  let make lst = Array.of_list lst

  let matrix_of_guess _ = Array.make_matrix ~dimx:5 ~dimy:12 black

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
    assert(equals (sexual_reproduction 0. g5 g6) (make [r2; r3]))

  let run_tests () =
    test_sexual_reproduction ();
    () 
end

(* Applies the functor to make a Guess using our implementation of Polygon *)
(* module Guess : GUESS = MakePolygonGuess(Polygon) *)

(* Applies the functor to make a Guess using our implementation of Circle *)
module Guess : GUESS = MakeCircleGuess(Circle)

