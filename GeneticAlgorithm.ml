open Core.Std
open Guess

(* Signature of a generic Genetic Algorithm Module *)
module type GENETIC_ALGORITHM =
sig
  (* The type of a guess*)
  type guess
  
  (* Type of the Genetic Algorithm *)
  type ga
  
  (* fresh n m returns a fresh new Genetic Algorithm with n guesses 
   * each containing m polygons *)
  val fresh : int -> int -> ga

  (* 'evolve g n' performs the genetic algorithm for n generations *)
  val evolve : ga -> int -> ga
  
  (* Eliminates the unfit *)
  val kill_phase : ga -> ga
  
  (* Performs random reproduction on the fitter part of the sample *)
  val reproduction_phase : ga -> ga
  
  (* Returns the current best guess of this model *)
  val get_best : ga -> guess
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module MakeGeneticAlgorithm (G : GUESS) : GENETIC_ALGORITHM with type guess = G.guess =
struct
  type guess = G.guess

  type ga = guess array

  (* Returns a list of N random guesses *)
  let fresh n m =
    let init_guess i = G.fresh m in
    Array.init n init_guess
  
  let kill_phase g = failwith "TODO"

  let reproduction_phase g = failwith "TODO"
  
  (* evolve simply calls kill phase and reproduction phase on g n times *)
  let rec evolve g n =
    if n <= 0 then g 
    else evolve (reproduction_phase (kill_phase g)) (n - 1)
  
  
  
  let get_best g = failwith "TODO"
    

  let run_tests () =
    ()
    
end