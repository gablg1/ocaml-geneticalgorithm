open Core.Std
open Guess
open Graphics

(* Signature of a generic Genetic Algorithm Module *)
module type GENETIC_ALGORITHM =
sig
  (* The type of a guess*)
  type guess
  
  (* Type of the Genetic Algorithm *)
  type ga
  
  (* fresh n m v returns a fresh new Genetic Algorithm with n guesses 
   * each containing m polygons with v vertices *)
  val fresh : int -> int -> int -> ga

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

  type ga = guess array * image
  
  let guesses (gs,_) = gs
  
  let target (_,img) = img

  (* Returns a list of N random guesses *)
  let fresh n m v =
    (* Placeholder image *)
    let width, height = 1, 1 in
    let target = Graphics.create_image width height in
    
    (* Initializes the random guesses *)
    (Array.init n ~f:(fun _ -> G.fresh width height m v), target)
  
  let kill_phase _ = failwith "TODO"

  let reproduction_phase _ = failwith "TODO"
  
  (* evolve simply calls kill phase and reproduction phase on g n times *)
  let rec evolve g n =
    if n <= 0 then g 
    else evolve (reproduction_phase (kill_phase g)) (n - 1)

  let get_best _ = failwith "TODO" 

  let run_tests () =
    ()
    
end
