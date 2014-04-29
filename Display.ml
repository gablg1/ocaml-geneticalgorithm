(* With CamlImages

let () =
  let img = Rgb24.create 1 1 in
  Rgb24.set img 0 0 { Color.r = 255; g = 0; b = 0 };
  Bmp.save "sample.bmp" [] (Images.Rgb24 img)

*)

(* Without CamlImages - Display *)

#load "graphics.cma"
open Graphics
open Array
open Printf

(* Takes a color array array, and iteration number of reproduction. *)
let draw_from_colors matrix i = 
  let img = Graphics.make_image matrix in
  let title = "Polygon Reproduction: Iteration " ^ (string_of_int i) in 
  Graphics.set_window_title title; Graphics.draw_image img 0 0
;;

(* testing *)
let color1 = Graphics.rgb 100 150 300 in
let m = Array.make_matrix 200 300 color1 in
Graphics.open_graph " 200x400"; 
draw_from_colors m 4
;;


let save_img_file matrix i =
  let file = "iteration[" ^ (string_of_int i) ^ "].ml" in
  let message = 
    (string_of_int i) ^ "/n"
    for i = 0 to (Array.length matrix) do
    (Array.fold_right ~f(fun x y -> (string_of_int x) ^ " " ^ y) ~init:"") ^ "/n"
    done 
  in
  let oc = open_out file in   
  fprintf oc "%s\n" message; close_out_noerr oc
;;              
  
let show_img_file file =
  let ic = open_in file in
  let line = input_line ic in  
  print_endline line; flush stdout; close_in_noerr ic;
  (* let i = first line, then skip the /n, then make a matrix named m
let m = (make from matrices?), and finally: 
Graphics.open_graph " 200x400"; draw_from_colors m i*)
;;              


