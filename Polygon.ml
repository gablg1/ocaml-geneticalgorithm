open Core.Std
open Graphics
open Statistics
open Helpers.Helpers

(* Module that performs operations on Polygons *)
module type POLYGON =
sig
  type polygon

  (* Returns the list of points of the polygon *)
  val points : polygon -> (float * float) array
  
  (* 'fresh a b v' returns a fresh randomly initialized polygon with v vertices
   * and points all within the rectangle delimited by (0,0) (a, b) *)
  val fresh : int -> int -> int -> polygon
  
  (* Makes a new polygon out of a point list and a color *)
  val make : (float * float) list -> color -> polygon
  
  (* Returns the color of the polygon as an RGB triple *)
  val rgb : polygon -> (int * int * int)
  
  (* Returns the color of the polygon *)
  val color : polygon -> color
  
  (* 'sexual_reproduction p1 p2 c' returns a polygon
   * daughter of p1 and p2 with crossing over level of c *)
  val sexual_reproduction : float -> polygon -> polygon -> polygon
  
  (* Returns true if two polygons are equal and false otherwise *)
  val equals : polygon -> polygon -> bool
  
  (* Prints a polygon *)
  val print : polygon -> unit
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module Polygon : POLYGON =
struct
  
  type polygon = (float * float) array * color

  let fresh a b v = 
    let fa, fb = Float.of_int a, Float.of_int b in
    let points = Array.init v ~f:(fun _ -> random_point fa fb) in
    (points, random_color ())

  let make (lst : (float * float) list) (c : color) = (Array.of_list lst, c)
  
  let points (ps,_) = ps

  let color (_,c) = c

  let rgb (_,c) = to_rgb c

  let point_reproduction std_dev p1 p2 =
    let (x,y) = halfway_point p1 p2 in
    (Statistics.gaussian_pick std_dev x, Statistics.gaussian_pick std_dev y)

  let color_reproduction std_dev c1 c2 =
    let f a1 a2 = Int.of_float (Statistics.gaussian_pick std_dev ((Float.of_int (a1 + a2)) /. 2.0)) in
    let (r1,g1,b1) = to_rgb c1
    and (r2,g2,b2) = to_rgb c2 in
    Graphics.rgb (f r1 r2) (f g1 g2) (f b1 b2)
    
  let sexual_reproduction std_dev p1 p2 = 
    let points = Array.map2_exn (points p1) (points p2) ~f:(point_reproduction std_dev) in
    (points, color_reproduction std_dev (color p1) (color p2))

  let test_points () =
    let p1 = make [] blue in
    let p2 = make [(0.,0.)] red in
    let p3 = make [(5.,12.);(4.,9.);(9.,1.);(9.,3.)] green in
    assert(points p1 = Array.of_list []);
    assert(points p2 = Array.of_list [(0.,0.)]);
    assert(points p3 = Array.of_list [(5.,12.);(4.,9.);(9.,1.);(9.,3.)])

  let test_color () =
    let p1 = make [] blue in
    let p2 = make [(0.,0.)] red in
    let p3 = make [(5.,12.);(4.,9.);(9.,1.);(9.,3.)] green in
    assert(color p1 = blue);
    let r,g,b = rgb p1 in
    assert(Graphics.rgb r g b = blue);
    assert(color p2 = red);
    let r,g,b = rgb p2 in
    assert(Graphics.rgb r g b = red);
    assert(color p3 = green);
    let r,g,b = rgb p3 in
    assert(Graphics.rgb r g b = green)

  (* Prints a polygon for testing purposes *)
  let print p =
    print_endline "######### Start of Polygon #########";
    Array.iter ~f:(fun p -> print_point p) (points p);
    Printf.printf "Color: %x\n" (color p);
    print_endline "######### End of Polygon #########"

  let equals p1 p2 =
    points p1 = points p2 && color p1 = color p2

  let test_sexual_reproduction () =
    let p1 = make [(0.,0.);(1.,1.);(2.,2.);(3.,3.)] blue in
    let p2 = make [(1.,2.);(3.,4.);(5.,6.);(7.,8.)] red in
    let p3 = make [(5.,12.);(4.,9.);(9.,1.);(9.,3.)] green in
    
    let r1 = make [(0.5,1.);(2.,2.5);(3.5,4.);(5.,5.5)] (halfway_color blue red) in
    let r2 = make [(3.,7.);(3.5,6.5);(7.,3.5);(8.,5.5)] (halfway_color red green) in
    let r3 = make [(2.5,6.);(2.5,5.);(5.5,1.5);(6.,3.)] (halfway_color blue green) in
    
    assert(equals (sexual_reproduction 0. p1 p2) r1);
    assert(equals (sexual_reproduction 0. p2 p3) r2);
    assert(equals (sexual_reproduction 0. p1 p3) r3)

  let run_tests () =
    test_points ();
    test_color ();
    test_sexual_reproduction ();
    ()

end

