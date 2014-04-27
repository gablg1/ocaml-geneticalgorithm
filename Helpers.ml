open Core.Std
open Graphics

(* Generic helpers used throughout the program *)
module type HELPERS =
sig
  (* apply_point f (x, y) returns (f x, f y) *)
  val apply_point : ('a -> 'b) -> ('a * 'a) -> ('b * 'b)

  (* Returns rgb triple of color *)
  val to_rgb : color -> (int * int * int)
  
  (* halfway_points p1 p2 returns a point that's halfway between p1 and p2 *)
  val halfway_point : (float * float) -> (float * float) -> (float * float)
  
  (* halfway_color c1 c2 returns a color that's halfway between c1 and c2 *)
  val halfway_color : color -> color -> color
  
  (* Prints a point *)
  val print_point : (float * float) -> unit
  
  (* Generates a random point within (a,b) *)
  val random_point : float -> float -> (float * float)
  
  (* Generates a random color *)
  val random_color : unit -> color
end

(* Implements Helpers *)
module Helpers : HELPERS  =
struct
  let apply_point f pt = 
    let (x, y) = pt in 
    (f x, f y)
  
  let to_rgb c =
    let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in (r,g,b)

  let halfway_point (x1,y1) (x2,y2) =
    ((x1 +. x2) /. 2., (y1 +. y2) /. 2.)
    
  let halfway_color c1 c2 =
    let f a1 a2 = ((a1 + a2) / 2) in
    let (r1,g1,b1) = to_rgb c1
    and (r2,g2,b2) = to_rgb c2 in
    Graphics.rgb (f r1 r2) (f g1 g2) (f b1 b2)
    
  let print_point (x,y) =
    Printf.printf "(%f, %f)\n" x y
    
  let random_point a b = ((Random.float a), Random.float b)
  
  let random_color () = Random.int 0xffffff
end


