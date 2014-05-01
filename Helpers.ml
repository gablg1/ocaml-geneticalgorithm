open Core.Std
open Graphics

(* Generic helpers used throughout the program *)
module type HELPERS =
sig
  exception DimensionTrouble

  (* valid (w,h) (x,y) returns whether point (x,y) is within
   * the boundaries of (0,0) (w,h) *)
  val valid_point : (int * int) -> (float * float)  -> bool
  
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
  
  (* Returns the number of elements of a matrix *)
  val count_mat : 'a array array -> int
  
  (* Returns the absolute difference between two colors*)
  val diff_color : color -> color -> int
  
  (* Returns the sum of an int array *)
  val sum_array : int array -> int
  
  (* diff_array f a1 a2 returns the difference between arrays
   * a1 and a2 where the difference between two elements x y is 
   * given by f x y *)
  val diff_array : ('a -> 'a -> int) -> 'a array -> 'a array -> int
  
  (* diff_mat f aa1 aa2 returns the difference between matrices
   * aa1 and aa2 where the difference between two elements x y is 
   * given by f x y *)
  val diff_mat : ('a -> 'a -> int) -> 'a array array -> 'a array array -> int
  
  (* Returns an array array out of an upside down list list *)
  val matrix_of_list_list_rev : 'a list list -> 'a array array
end

(* Implements Helpers *)
module Helpers : HELPERS  =
struct
  exception DimensionTrouble

  let matrix_of_list_list_rev ll =
    let a = Array.of_list (List.rev ll) in
    Array.map ~f:(fun l -> Array.of_list (l)) a

  let valid_point (w,h) (x,y) = x > 0. && y > 0. && x < Float.of_int w && y < Float.of_int h

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
  
  (* Stephen's stuff *)
  let count_mat (iaa : 'a array array) : int =
    (Array.length iaa) * (Array.length iaa.(0))
    
  (* helper function that compares colors and gives a cost value for the two *)
  (* Lower the cost the  better *)  
  let diff_color (c1 : color) (c2 : color) : int =  
    let (r1, b1, g1) = to_rgb c1 in 
    let (r2, b2, g2) = to_rgb c2 in 
    abs(r2 - r1) + abs(b2 - b1) + abs(g2 - g1)
  
  let sum_array (ia : int array) : int = Array.fold_right ia ~f:(+) ~init:0
  
  let diff_array (f : 'a -> 'a -> int) (ca1 : 'a array) (ca2 : 'a array) : int =
    let n1 = Array.length ca1
    and n2 = Array.length ca2 in
    if n1 <> n2 then raise DimensionTrouble else
      
    let result = Array.create (n1) 0 in
    for i = 0 to n1 - 1 do result.(i) <- (f ca1.(i) ca2.(i)) done; 
    sum_array result
  
  let diff_mat (f : 'a -> 'a -> int) (aa1 : 'a array array) (aa2 : 'a array array) : int = 
    let diffs = Array.map2_exn aa1 aa2 ~f:(diff_array f) in
    sum_array diffs
  
end


