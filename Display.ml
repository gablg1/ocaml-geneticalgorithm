(* With CamlImages

let () =
  let img = Rgb24.create 1 1 in
  Rgb24.set img 0 0 { Color.r = 255; g = 0; b = 0 };
  Bmp.save "sample.bmp" [] (Images.Rgb24 img)

*)

(* Without CamlImages - Display *)

#load "graphics.cma"
open Graphics
open Array

(* Takes a color array array, and iteration number of reproduction.
 *)

let draw_from_colors matrix i = 
  let img = Graphics.make_image matrix in
  let title = "Polygon Reproduction: Iteration " ^ (string_of_int i) in 
  Graphics.set_window_title title; Graphics.draw_image img 0 0
;;

(* testing *)
let color1 = Graphics.rgb 100 150 300 in
let m = Array.make_matrix 200 300 color1 in
Graphics.open_graph " 200x400"; 
draw_from_colors m 4
;;

