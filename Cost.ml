#load "graphics.cma"

open Core.Std
open Graphics
open Array 
open Helpers
open Circle 
open Images

(*open Images
open OImages*)
(* XXX Remember to open the CamlImages file
 * converts a file to an image that we can use *) 

(* converts a color to an rgb file *)  
 let to_rgb c =
    let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in (r,g,b)
;; 

(* helper function that compares colors and gives a cost value for the two *)
(* Lower the cost the  better *)  
let compare_colors (c1 : color) (c2 : color) : int =  
  let (r1, b1, g1) = to_rgb c1 in 
  let (r2, b2, g2) = to_rgb c2 in 
  abs(r2 - r1) + abs(b2 - b1) + abs(g2 - g1) 
;; 


(* takes in two color arrays and compares them point by point *) 
let compare_c_array (ca1 : color array) (ca2 : color array) : int array =
  let n1 = Array.length ca1
  and n2 = Array.length ca2 in
  let result = Array.create (max n1 n2) 0 in
  for i = 0 to n1 - 1 do result.(i) <- (compare_colors ca1.(i) ca2.(i)) done; 
  result;;

(* compares two color array array point by point and returns an int matrix *) 
let compare_pixmap (caa1 : color array array) (caa2 : color array array) : int array array = 
  Array.map2_exn caa1 caa2 ~f:(compare_c_array)
(*let n1 = Array.length caa1
  and n2 = Array.length caa2 in
  let result = Array.create (max n1 n2) caa1.(0) in
  for i = 0 to n1 - 1 do result.(i) <- (compare_c_array caa1.(i) caa2.(i)) done;
  result;; *)

(* counts the number of elements in an array array, used to average cost functions *) 
(*let counts_array (iaa : int array array) : int = 
  let n1 = Array.length iaa in 
  let result = ref 0 in 
  for i = 0 to n1 - 1 do result := ((Array.length iaa.(i)) + !result) done;
  !result;;
 *)
(* counts the number of items in the int matrix *) 
let counts_array (iaa : int array array) : int =
  (Array.length iaa) * (Array.length iaa.(0))
			(* Array.fold_right iaa  ~f:(fun x rest -> (Array.length x) + rest) ~init:0  *)
;; 
(* sums all of the costs in an array *) 
let sum_array (ia : int array) : int = 
   Array.fold_right ia ~f: (+) ~init:0 

(* gives the average cost function of the int matrix as a float *) 
let average_array (iaa : int array array) : float = 
  let total =  Array.fold_right iaa ~f:(fun x rest -> (sum_array x) + rest) ~init:0 in 
  let total' = float total in 
  let num_el = float (counts_array iaa) in 
  total' /. num_el 

(* returns the fitness of a color matrix compared to another color matrix *)   
let cost_of_mat (caa1 : color array array) (caa2 : color array array) : float = 
  let cost_mat = compare_pixmap caa1 caa2 in 
  average_array cost_mat


(* Some test functions for the functions above *)  
let blue_list_list = Array.create 2 (Array.create 2 blue) ;; 
let red_list_list = Array.create 2 (Array.create 2 red) ;;
assert((counts_array red_list_list) = 4);
assert((compare_pixmap blue_list_list red_list_list = [|[|510; 510|]; [|510; 510|]|])); 
assert((cost_of_mat blue_list_list red_list_list = 510.0));
assert(( cost_of_mat blue_list_list blue_list_list = 0.0));

let green_list_list = Array.create 2 (Array.create 3 green) in 
cost_of_mat blue_list_list green_list_list;;  
let array1 = Array.create 2 2 in 
assert(sum_array array1 = 4); 
let mat1 = Array.create 2 (Array.create 2 2) in 
let mat2 = Array.create 2 (Array.create 2 4) in 
assert(average_array mat1 = 2.0);; 


(* takes in two colors and then gives back a color that is the average of the two *) 
let color_combiner (c1 : color) (c2 : color) : color = 
  let (r1, b1, g1) = to_rgb c1 in 
  let (r2, b2, g2) = to_rgb c2 in 
  let r3 =  ((r2 + r1) / 2) in 
  let b3 =  ((b2 + b1) / 2) in 
  let g3 =  ((g2 + g1) / 2) in 
  rgb r3 b3 g3  
;; 

(* creates a black color matrix *) 
let blank_matrix w h : color array array  = Array.make_matrix ~dimx:w ~dimy:h 0 ;; 


 
(* updates the matrix for each circle that is passed in *)  
let matrix_helper (caa : color array array) (c : Circle.circle) : color array array = 
  let n1 = Array.length caa 
  and n2 = Array.length caa.(0) in
  for i = 0 to n1 - 1 do 
  for j = 0 to n2 - 1 do  
  let curr_array = caa.(i) in 
  if Circle.contains c ((float j),(float i)) then 
  Array.set curr_array j (color_combiner (color c) curr_array.(j)) done done;
  caa ;;     


(* takes in a guess and passes out the color matrix that represents that guess *)
let matrix_of_guess (g : guess) : color array array = 
  let w = width g in 
  let h = height g in                      
  let matrix = blank_matrix w h  in 
  let circles =  Guess.legos g in 
  (* can I simply make the function update_helper caa or do I need to pass inthe other argument? *) 
  Array.fold_right circles ~f:(matrix_helper caa) ~init:0  




(* 
1. update_helper will take in a circle and then 
2. update_matrix takes in a guess, and then iterates through the circles using update helper 
to update the color matrix that we have 

 *) 
