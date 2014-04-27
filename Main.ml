open Core.Std
open GeneticAlgorithm
open Guess
open Polygon
open Statistics

(* Initializes randomness using current timestamp *)
let _ = Random.init (Float.to_int (Unix.time ())) in

Polygon.run_tests ();;
Guess.run_tests ();;
GeneticAlgorithm.run_tests ();;


