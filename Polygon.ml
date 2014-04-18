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

  let points (ps,c) = ps

  let color (ps,c) = c

  let sexual_reproduction p1 p2 = (points p1, (color p1 + (color p2)) / 2)

  let run_tests () = 

end

