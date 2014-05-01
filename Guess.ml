open Core.Std
open Graphics
open Helpers.Helpers
open Polygon
open Circle

let max_fitness = 255. *. 3.

module type GUESS =
sig
  (* Raised when there is incompatibility of width/height *)
  exception DimensionTrouble
  exception CostTrouble

  (* lego is the building block of the image *)
  type lego

  type guess

  (* fresh w h m returns a new random guess with m legos 
   * where w and h are the width and height of the target image *)
  val fresh : int -> int -> int -> guess

  (* Returns the width of the guess *)
  val width : guess -> int
  
  (* Returns the height of the guess *)
  val height : guess -> int

  (* Returns list of legos that composes guess *)
  val legos : guess -> lego array 

  (* Returns guess as color array array *)
  val matrix_of_guess : guess -> color array array

  (* Draws guess to the screen *)
  val draw : guess -> unit
  
  (* Measures the fitness of the guess against the target.
   * The greater the weight (between 0 and max_fitness) the better. *)
  val fitness : color array array -> guess -> float

  (* Makes a new guess out of width, height, and a lego list *)
  val make : int -> int -> lego list -> guess

  (* Performs sexual reproduction between this guess and another guess
   * by calling reproduction on each of the correspondent legos.
   * Third argument is the level of chaos of the reproduction
   * Returns a daughter guess *)
  val sexual_reproduction : float -> guess -> guess -> guess

  (* Same as above but with only one parent guess *)
  val asexual_reproduction : float -> guess -> guess

  (* Returns true if two guesses are equal and false otherwise *)
  val equals : guess -> guess -> bool

  (* Prints a Guess *)
  val print : guess -> unit
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module MakeCircleGuess (C : CIRCLE) : GUESS  =
struct
  exception DimensionTrouble
  exception CostTrouble
  
  type lego = C.circle
  
  (* Stores guess as lego array + (width, height) *)
  type guess = int * int * lego array
  
  let fresh width height m = (width, height, Array.init ~f:(fun _ -> C.fresh width height) m)
  
  let legos (_,_,ls) = ls
  
  let width (w,_,_) = w
  
  let height (_,h,_) = h

  let make w h lst = (w,h, Array.of_list lst)
  
 (* updates the matrix for each circle that is passed in *)  
  let insert_circle  (caa : color array array) (c : lego) : color array array = 
    let n1 = Array.length caa 
    and n2 = Array.length caa.(0) in
    for i = 0 to n1 - 1 do 
    for j = 0 to n2 - 1 do  
    let curr_array = caa.(i) in 
    if C.contains c ((float j),(float i)) then 
    Array.set curr_array j (halfway_color (C.color c) curr_array.(j)) done done;
    caa;;     


  (* takes in a guess and passes out the color matrix that represents that guess *)
  let matrix_of_guess (g : guess) : color array array = 
    let w = width g in 
    let h = height g in                      
    let matrix = blank_matrix w h  in 
    let circles =  legos g in 
    
    Array.fold_right circles ~f:(fun c rest -> insert_circle rest c) ~init:matrix

  let draw g = 
    let img = Graphics.make_image (matrix_of_guess g) in
    Graphics.draw_image img 100 300
  
  let dimensions_agree g1 g2 = (width g1 = width g2) && height g1 = height g2

  (* returns the fitness of a color matrix compared to another color matrix *)   
  let cost_of_mat (caa1 : color array array) (caa2 : color array array) : float = 
    let cost_mat = diff_mat diff_color caa1 caa2 in 
    let num_el = count_mat caa1 in
    Float.of_int(cost_mat) /. Float.of_int (num_el)

  let fitness target guess = 
    let cost = cost_of_mat (matrix_of_guess guess) target in
    
    if cost > max_fitness then raise CostTrouble
    else Float.abs (max_fitness -. cost)

  let sexual_reproduction std_dev g1 g2 = 
    if not (dimensions_agree g1 g2) then raise DimensionTrouble 
    else ((width g1), (height g1), Array.map2_exn ~f:(C.sexual_reproduction std_dev) (legos g1) (legos g2))
  
  let asexual_reproduction std_dev g1 =
    ((width g1), (height g1), Array.map ~f:(C.asexual_reproduction std_dev) (legos g1))
  
  let equals g1 g2 = 
    dimensions_agree g1 g2 &&
    Array.fold2_exn ~f:(fun r p1 p2 -> (C.equals p1 p2) && r) ~init:(true) (legos g1) (legos g2)
  
  let print g =
    print_endline "################ Start of Guess ################";
    printf "Width: %i Height: %i\n" (width g) (height g);
    Array.iter ~f:(C.print) (legos g);
    print_endline "################ End of Guess ################";
    print_endline ""
  
  let test_sexual_reproduction () =
    let p1 = C.make (100,100) (0.,0.) 3. blue in
    let p2 = C.make (100,100) (1.,2.) 4. red in
    let p3 = C.make (100,100) (3.,4.) 5. green in
    
    let g1 = make 100 100 [p1] in
    let g2 = make 100 100 [p2] in
    let g3 = make 100 100 [p3] in
    let g4 = make 100 100 [p1; p2] in
    let g5 = make 100 100 [p2; p3] in
    let g6 = make 100 100 [p3; p1] in
    
    (* Answers *)
    let r1 = C.make (100,100) (0.5,1.) 3.5 (halfway_color blue red) in
    let r2 = C.make (100,100) (2.,3.) 4.5 (halfway_color red green) in
    let r3 = C.make (100,100) (1.5,2.) 4. (halfway_color blue green) in
    
    assert(equals (sexual_reproduction 0. g1 g2) (make 100 100 [r1]));
    assert(equals (sexual_reproduction 0. g2 g3) (make 100 100 [r2]));
    assert(equals (sexual_reproduction 0. g1 g3) (make 100 100 [r3]));
    assert(equals (sexual_reproduction 0. g4 g5) (make 100 100 [r1; r2]));
    assert(equals (sexual_reproduction 0. g4 g6) (make 100 100 [r3; r1]));
    assert(equals (sexual_reproduction 0. g5 g6) (make 100 100 [r2; r3]))
  
  (* tests the cost function that we created *) 
  let test_cost () =  

  let blue_array_array = Array.create ~len:2 (Array.create ~len:2 blue) in 
  let red_array_array = Array.create ~len:2 (Array.create ~len:2 red) in 
  
  assert(cost_of_mat blue_array_array red_array_array = 510.);
  assert(cost_of_mat blue_array_array blue_array_array = 0.);
  ()

  let run_tests () =
    test_sexual_reproduction ();
    test_cost (); 
    () 
end

(* Not in use *)
module MakePolygonGuess (P : POLYGON) : GUESS =
struct
  exception DimensionTrouble
  exception CostTrouble

  type lego = P.polygon
  type guess = int * int * lego array
  
  let fresh_v width height m v = width, height, Array.init ~f:(fun _ -> P.fresh width height v) m
  
  (* Number of vertices is always 1 *)
  let fresh w h m = fresh_v w h m 1
  
  let legos (_,_,ls) = ls
  
  let width (w,_,_) = w
  
  let height (_,h,_) = h

  let draw = failwith "TODO"

  let make w h lst = (w,h, Array.of_list lst)
  
  let dimensions_agree g1 g2 = (width g1 = width g2) && height g1 = height g2

  let matrix_of_guess _ = Array.make_matrix ~dimx:5 ~dimy:12 black

  let fitness _ _ = failwith "TODO"

  let sexual_reproduction std_dev g1 g2 = 
    if not (dimensions_agree g1 g2) then raise DimensionTrouble 
    else ((width g1), (height g1), Array.map2_exn ~f:(P.sexual_reproduction std_dev) (legos g1) (legos g2))
  
  let asexual_reproduction std_dev g1 = sexual_reproduction std_dev g1 g1
  
  let equals g1 g2 = 
    dimensions_agree g1 g2 &&
    Array.fold2_exn ~f:(fun r p1 p2 -> (P.equals p1 p2) && r) ~init:(true) (legos g1) (legos g2)
  
  let print g =
    print_endline "################ Start of Guess ################";
    Array.iter ~f:(P.print) (legos g);
    print_endline "################ End of Guess ################";
    print_endline ""
  
  let test_sexual_reproduction () =
    let p1 = P.make [(0.,0.);(1.,1.);(2.,2.);(3.,3.)] blue in
    let p2 = P.make [(1.,2.);(3.,4.);(5.,6.);(7.,8.)] red in
    let p3 = P.make [(5.,12.);(4.,9.);(9.,1.);(9.,3.)] green in
    let g1 = make 100 100 [p1] in
    let g2 = make 100 100 [p2] in
    let g3 = make 100 100 [p3] in
    let g4 = make 100 100 [p1; p2] in
    let g5 = make 100 100 [p2; p3] in
    let g6 = make 100 100 [p3; p1] in
    
    (* Answers *)
    let r1 = P.make [(0.5,1.);(2.,2.5);(3.5,4.);(5.,5.5)] (halfway_color blue red) in
    let r2 = P.make [(3.,7.);(3.5,6.5);(7.,3.5);(8.,5.5)] (halfway_color green red) in
    let r3 = P.make [(2.5,6.);(2.5,5.);(5.5,1.5);(6.,3.)] (halfway_color blue green) in
    
    assert(equals (sexual_reproduction 0. g1 g2) (make 100 100 [r1]));
    assert(equals (sexual_reproduction 0. g2 g3) (make 100 100 [r2]));
    assert(equals (sexual_reproduction 0. g1 g3) (make 100 100 [r3]));
    assert(equals (sexual_reproduction 0. g4 g5) (make 100 100 [r1; r2]));
    assert(equals (sexual_reproduction 0. g4 g6) (make 100 100 [r3; r1]));
    assert(equals (sexual_reproduction 0. g5 g6) (make 100 100 [r2; r3]))

  let run_tests () =
    test_sexual_reproduction ();
    () 
end

(* Applies the functor to make a Guess using our implementation of Polygon *)
(* module Guess : GUESS = MakePolygonGuess(Polygon) *)

(* Applies the functor to make a Guess using our implementation of Circle *)
module Guess : GUESS = MakeCircleGuess(Circle)

