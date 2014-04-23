open Core.Std
open GeneticAlgorithm
open Guess
open Polygon
open Cost
open Statistics

(* Initializes randomness using current timestamp *)
let _ = Random.init (Float.to_int (Unix.time ())) in

Polygon.run_tests ();;


