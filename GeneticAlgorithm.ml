open Core.Std
open Guess
open Helpers.Helpers
open Graphics

(* Signature of a generic Genetic Algorithm Module *)
module type GENETIC_ALGORITHM =
sig
  (* The type of a guess*)
  type guess
  
  (* Type of the Genetic Algorithm *)
  type ga
  
  (* Returns the list of guesses *)
  val guesses : ga -> guess array
  
  (* fresh s n m returns a fresh new Genetic Algorithm with Standard Deviation s
   * and n guesses each containing m legos *)
  val fresh : float -> color array array -> int -> int -> ga

  (* 'evolve g n' performs the genetic algorithm for n generations *)
  val evolve : ga -> int -> ga
  
  (* Eliminates the unfit *)
  val kill_phase : ga -> ga
  
  (* Performs random reproduction on the fitter part of the sample *)
  val reproduction_phase : ga -> ga
  
  (* Returns the current best guess of this model *)
  val get_best : ga -> guess
  
  (* Draws the best guess of this model *)
  val draw_best : ga -> unit
  
  (* Prints the current state of this genetic algorithm *)
  val print : ga -> unit
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module MakeImageGeneticAlgorithm (G : GUESS) : GENETIC_ALGORITHM with type guess = G.guess =
struct
  type guess = G.guess

  type ga = float * guess array * color array array
  
  let guesses (_,gs,_) = gs
  
  let target (_,_,img) = img

  let std_dev (s,_,_) = s
  
  (* Initializes the random guesses *)
  let fresh s t n m =
    let width, height = get_width t, get_height t in
    (s, Array.init n ~f:(fun _ -> G.fresh width height m), t)
  
  let kill_phase ga = ga

  let reproduction_phase (ga : ga) : ga = 
    (std_dev ga, Array.map ~f:(G.asexual_reproduction (std_dev ga)) (guesses ga), target ga)
  
  (* evolve simply calls kill phase and reproduction phase on g n times *)
  let rec evolve g n =
    if n <= 0 then g 
    else evolve (reproduction_phase (kill_phase g)) (n - 1)

  let get_best g = (guesses g).(0)
  
  let draw_best g = G.draw (get_best g)
  
  let print ga =
    print_endline "################### Start of Genetic Algorithm ###################";
    Array.iter ~f:(G.print) (guesses ga);
    print_endline "################### End of Genetic Algorithm ###################";
    print_endline ""

  let run_tests () =
   (* Since this is a probabilistic model, we test by printing *)
   (* let ga = fresh 10. 1 2 in
    print (ga);
    let evolved = evolve ga 10 in
    print evolved;*)
    ()
    
end

(* Applies the functor to make a GeneticAlgorithm using our implementation of Guess *)
module GeneticAlgorithm : GENETIC_ALGORITHM = MakeImageGeneticAlgorithm(Guess) 
