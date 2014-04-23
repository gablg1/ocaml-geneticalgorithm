open Core.Std
open Graphics
open Statistics

(* Module that performs operations on Polygons *)
module type POLYGON =
sig
  type polygon

  (* Returns the list of points of the polygon *)
  val points : polygon -> (int * int) list
  
  (* 'fresh a b' returns a fresh randomly initialized polygon with points all 
   * within the rectangle delimited by (0,0) (a, b) *)
  val fresh : int -> int -> polygon
  
  (* Returns the color of the polygon *)
  val color : polygon -> color
  
  (* Performs sexual reproduction between two polygons
   * outputing a daughter polygon *)
  val sexual_reproduction : polygon -> polygon -> polygon
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module Polygon : POLYGON with type polygon=((int * int) list) * color =
struct

  type polygon = ((int * int) list) * color

  let fresh _ _ = failwith "TODO"

  let points (ps,_) = ps

  let color (_,c) = c

  let halfway (x1,y1) (x2,y2) =
    ((x1 + x2) / 2, (y1 + y2) / 2)

  let to_rgb c =
    let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in (r,g,b)

  let sexual_reproduction p1 p2 = let (r1,g1,b1) = to_rgb (color p1) and (r2,g2,b2) = to_rgb (color p2) in
				  let f = fun c1 c2 -> Int.of_float (Statistics.gaussian_pick
				   (Float.of_int (c1 + c2) /. 2.0) 0.0) in
				  (List.map2_exn (points p1) (points p2) ~f:halfway,
				   rgb (f r1 r2) (f g1 g2) (f b1 b2))

  let test_points () =
    let p1 = ([], blue) in
    let p2 = ([(0,0)], red) in
    let p3 = ([(5,12);(4,9);(9,1);(9,3)], green) in
    assert(points p1 = []);
    assert(points p2 = [(0,0)]);
    assert(points p3 = [(5,12);(4,9);(9,1);(9,3)])

  let test_color () =
    let p1 = ([], blue) in
    let p2 = ([(0,0)], red) in
    let p3 = ([(5,12);(4,9);(9,1);(9,3)], green) in
    assert(color p1 = blue);
    assert(color p2 = red);
    assert(color p3 = green)

  let test_sexual_reproduction () =
    let p1 = ([(0,0);(1,1);(2,2);(3,3)], blue) in
    let p2 = ([(1,2);(3,4);(5,6);(7,8)], red) in
    let p3 = ([(5,12);(4,9);(9,1);(9,3)], green) in
    let (r1,g1,b1) = to_rgb blue in
    let (r2,g2,b2) = to_rgb red in
    let (r3,g3,b3) = to_rgb green in
    assert(sexual_reproduction p1 p2 = ([(0,1);(2,2);(3,4);(5,5)], rgb ((r1+r2)/2) ((g1+g2)/2) ((b1+b2)/2)));
    assert(sexual_reproduction p2 p3 = ([(3,7);(3,6);(7,3);(8,5)], rgb ((r2+r3)/2) ((g2+g3)/2) ((b2+b3)/2)));
    assert(sexual_reproduction p1 p3 = ([(2,6);(2,5);(5,1);(6,3)], rgb ((r1+r3)/2) ((g1+g3)/2) ((b1+b3)/2)))

  let run_tests () =
    test_points ();
    test_color ();
    test_sexual_reproduction ();
    ()

end

