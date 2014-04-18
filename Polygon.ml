open Core.Std
open Graphics

(* Module that performs operations on Polygons *)
module type POLYGON =
sig
  type polygon

  (* Returns the list of points of the polygon *)
  val points : polygon -> (int * int) list
  
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

  let points (ps,_) = ps

  let color (_,c) = c

  let rec map2 f xs ys =
    match xs,ys with
    | [], [] -> []
    | x::xs',y::ys' -> (f x y)::(map2 f xs' ys')
    | _ -> failwith "Invalid input - lists must be of equal length"

  let halfway (x1,y1) (x2,y2) =
    ((x1 + x2) / 2, (y1 + y2) / 2)

  let sexual_reproduction p1 p2 = (map2 halfway (points p1) (points p2), (color p1 + (color p2)) / 2)

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
    assert(sexual_reproduction p1 p2 = ([(0,1);(2,2);(3,4);(5,5)],(blue+red)/2));
    assert(sexual_reproduction p2 p3 = ([(3,7);(3,6);(7,3);(8,5)],(red+green)/2));
    assert(sexual_reproduction p1 p3 = ([(2,6);(2,5);(5,1);(6,3)],(blue+green)/2))

  let run_tests () =
    test_points ();
    test_color ();
    test_sexual_reproduction ();
    ()

end

