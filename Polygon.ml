open Core.Std
open Graphics
open Statistics
open Helpers.Helpers

(* Module that performs operations on Polygons *)
module type POLYGON =
sig
  type polygon

  (* Returns the list of points of the polygon *)
  val points : polygon -> (int * int) list
  
  (* 'fresh a b' returns a fresh randomly initialized polygon with points all 
   * within the rectangle delimited by (0,0) (a, b) *)
  val fresh : int -> int -> polygon
  
  (* Makes a new polygon out of a point list and a color *)
  val make_polygon : (int * int) list -> color -> polygon
  
  (* Returns the color of the polygon as an RGB triple *)
  val rgb : polygon -> (int * int * int)
  
  (* Returns the color of the polygon *)
  val color : polygon -> color
  
  (* 'sexual_reproduction p1 p2 c' returns a polygon
   * daughter of p1 and p2 with crossing over level of c *)
  val sexual_reproduction : float -> polygon -> polygon -> polygon
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module Polygon : POLYGON with type polygon=(float * float) list * color =
struct
  
  type polygon = (float * float) list * color

  let fresh a b = failwith "TODO"

  let make_int lst = List.map ~f:(apply_point Int.of_float) lst
  
  let make_float lst = List.map ~f:(apply_point Float.of_int) lst

  let make_polygon (lst : (int * int) list) (c : color) = (make_float lst, c)

  let float_points (ps,_) = ps
  
  let points (ps,_) = make_int ps

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
    let points = List.map2_exn (float_points p1) (float_points p2) ~f:(point_reproduction std_dev) in
    (points, color_reproduction std_dev (color p1) (color p2))

  let test_points () =
    let p1 = make_polygon [] blue in
    let p2 = make_polygon [(0,0)] red in
    let p3 = make_polygon [(5,12);(4,9);(9,1);(9,3)] green in
    assert(points p1 = []);
    assert(points p2 = [(0,0)]);
    assert(points p3 = [(5,12);(4,9);(9,1);(9,3)])

  let test_color () =
    let p1 = ([], blue) in
    let p2 = ([(0,0)], red) in
    let p3 = ([(5,12);(4,9);(9,1);(9,3)], green) in
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
  let print_polygon p =
    print_endline "######### Start of Polygon #########";
    List.iter ~f:(fun p -> print_point p) (float_points p);
    Printf.printf "Color: %x\n" (color p);
    print_endline "######### End of Polygon #########"

  let test_sexual_reproduction () =
    let p1 = make_polygon [(0,0);(1,1);(2,2);(3,3)] blue in
    let p2 = make_polygon [(1,2);(3,4);(5,6);(7,8)] red in
    let p3 = make_polygon [(5,12);(4,9);(9,1);(9,3)] green in
    
    let daughter = (sexual_reproduction 0. p1 p2) in
    assert(points daughter = [(0,1);(2,2);(3,4);(5,5)]);
    assert(color daughter = halfway_color blue red);
    
    let daughter = (sexual_reproduction 0. p2 p3) in
    assert(points daughter = [(3,7);(3,6);(7,3);(8,5)]);
    assert(color daughter = halfway_color red green);
    
    let daughter = (sexual_reproduction 0. p1 p3) in
    assert(points daughter = [(2,6);(2,5);(5,1);(6,3)]);
    assert(color daughter = halfway_color blue green)

  let run_tests () =
    test_points ();
    test_color ();
    test_sexual_reproduction ();
    ()

end

