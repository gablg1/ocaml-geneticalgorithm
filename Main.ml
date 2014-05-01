open Core.Std
open GeneticAlgorithm
open Guess
open Polygon
open Statistics
open MonaLisa
open Helpers.Helpers

(* Initializes randomness using current timestamp *)
let _ = Random.init (Float.to_int (Unix.time ())) in

Polygon.run_tests ();;
Guess.run_tests ();;
(*GeneticAlgorithm.run_tests ();;
*)
let draw_from_colors matrix = 
  let img = Graphics.make_image matrix in
  Graphics.draw_image img (100) (100)

let color1 = Graphics.rgb 100 150 300 in
let m = Array.make_matrix 200 400 color1 in

Graphics.open_graph " 500x500";
Graphics.resize_window 500 500;
Graphics.auto_synchronize false;
draw_from_colors (matrix_of_list_list_rev MonaLisa.mona_lisa);

ignore (Graphics.read_key ());
Graphics.close_graph ()

