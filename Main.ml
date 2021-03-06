open Core.Std
open GeneticAlgorithm
open Guess
open Polygon
open Statistics
open MonaLisa
open Small
open Medium
open Helpers.Helpers

(* Initializes randomness using current timestamp *)
let _ = Random.init (Float.to_int (Unix.time ())) in

Polygon.run_tests ();;
Guess.run_tests ();;
GeneticAlgorithm.run_tests ();;

Graphics.open_graph " 500x500";
Graphics.resize_window 500 500;

(* Creates Mona Lisa Genetic Algorithm *)
let target = matrix_of_list_list_rev Medium.medium in
let ga = ref (GeneticAlgorithm.fresh 0.1 target 1 50) in
GeneticAlgorithm.print !ga;
draw target;

let loop () =  
  ga := GeneticAlgorithm.evolve !ga 100;
  GeneticAlgorithm.print !ga;
  GeneticAlgorithm.draw_best !ga;
in

while true do loop () done;;

ignore (Graphics.read_key ());
Graphics.close_graph ()

