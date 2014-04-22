open Core.Std
open GeneticAlgorithm
open Guess
open Polygon
open Statistics

(* Initializes randomness using current timestamp *)
let _ = Random.init (Float.to_int (Unix.time ())) in

Polygon.run_tests ();;

print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 1.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 10.));;
print_endline (Float.to_string (Statistics.gaussian_pick 3. 100.));;

