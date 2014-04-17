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



