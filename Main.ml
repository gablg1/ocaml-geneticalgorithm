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
GeneticAlgorithm.run_tests ();;

Graphics.open_graph " 500x500";
Graphics.resize_window 500 500;

(* Creates Mona Lisa Genetic Algorithm *)
let target = matrix_of_list_list_rev MonaLisa.mona_lisa in
let ga = GeneticAlgorithm.fresh 10. target 1 10 in
GeneticAlgorithm.print ga;

let ga = GeneticAlgorithm.evolve ga 100 in
GeneticAlgorithm.print ga;
GeneticAlgorithm.draw_best ga;

draw target;

ignore (Graphics.read_key ());
Graphics.close_graph ()

