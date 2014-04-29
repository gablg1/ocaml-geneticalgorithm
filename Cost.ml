#load "graphics.cma"

open Graphics
open Array 
open Core.Std
open Helper.Helper
(*open Images
open OImages*)
(* XXX Remember to open the CamlImages file
 * converts a file to an image that we can use *) 

type bitmap_state  = 
   {w : int; h : int; fg : Graphics.color; bg : Graphics.color;
    pix : bool array array; s : int} ;;

let create_bitmap x y f g s = 
   let r = Array.make_matrix x y false in 
   { w = x; h = y; fg = f;  bg = g; pix = r; s = s} ;;

let draw_pix i j s c = 
   Graphics.set_color c;
   Graphics.fill_rect (i*s+1) (j*s+1) (s-1) (s-1) ;;

let draw_bitmap b = 
   for i=0 to b.w-1 do 
     for j=0 to b.h-1 do 
        draw_pix i j b.s (if b.pix.(i).(j) then b.fg else b.bg)
     done
   done ;;

let read_file filename = 
   let ic = open_in filename in 
     let rec aux () = 
       try 
         let line = (input_line ic) in
         line :: (aux ())
       with End_of_file -> close_in_noerr ic ; [] 
   in aux ();;

let read_bitmap filename  = 
   let r = Array.of_list (read_file filename)  in 
   let h = Array.length r in 
   let w = String.length r.(0) in 
   let b = create_bitmap w h Graphics.black Graphics.white 10 in 
     for j = 0 to  h - 1 do 
       for i = 0 to w - 1 do 
         b.pix.(i).(j) <-  ( r.(j).[i] = '#')
       done
     done;
     b ;;

let write_bitmap filename b = 
   let oc = open_out filename in
   let message = "wrote a bitmap" in 
   fprintf oc "%s\n" message;     
   let f x = output_char oc (if x then '#' else '-')  in
   Array.iter b.pix (fun x -> (Array.iter x f); output_char oc '\n');
   close_out_noerr oc ;;

exception End ;;
let skel f_init f_end f_key f_mouse f_except = 
   f_init ();
   try 
     while true do 
       try 
         let s = Graphics.wait_next_event 
                   [Graphics.Button_down; Graphics.Key_pressed]  in 
         if s.Graphics.keypressed 
         then f_key s.Graphics.key
         else if s.Graphics.button 
              then f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
       with 
           End -> raise End
         |  e  -> f_except e
     done
   with 
     End -> f_end () ;;

let start b () = 
   let sw = 1+b.w*b.s and sh = 1+b.h*b.s in 
   Graphics.open_graph (" " ^ (string_of_int sw) ^ "x" ^ (string_of_int sh)) ;
   Graphics.set_color (Graphics.rgb 150 150 150) ;
   Graphics.fill_rect 0 0 sw sh ;
   draw_bitmap b ;;

let stop () = Graphics.close_graph() ; exit 0 ;;

let mouse b x y  = 
   let i,j = (x / b.s),(y/b.s) in 
   if ( i < b.w ) && ( j < b.h) then 
     begin
       b.pix.(i).(j) <- not b.pix.(i).(j) ;
       draw_pix i j b.s (if b.pix.(i).(j) then b.fg else b.bg)
     end ;;

let key filename b c = 
   match c with 
     'q' | 'Q' -> raise End
   | 's' | 'S' -> write_bitmap filename b
   | _ -> () ;;

let go name = 
   let b =  try   
              read_bitmap name  
            with 
               _ -> create_bitmap 10 10 Graphics.black Graphics.white 10 
   in skel (start b) stop (key name b) (mouse b) (fun e -> ()) ;;

(*let from_rgb (c : Graphics.color) = 
let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 
in (r,g,b);;
val from_rgb : Graphics.color -> int * int * int = <fun>*)

(*let file = "hello.bmp" in
let outfile = "out" ^ file in
let img = OImages.load file [] in
let img = OImages.rgb24 img in
let rgb_vals = img.to_rgb24 in
Printf.printf "%d" rgb_vals;;


let open_image_print i = 
  let rgb = Graphics.dump_image i in
  let first_pixel = Array.get (Array.get rgb 0) 0 in
  Printf.printf "%d" first_pixel in
open_image_print oimage *)

(*
(* Opens an image and prints out the first pixel *) 
fun open_image_print (i : image) : int -> 
  let image = Graphics.dump_image i in 
  Array.get 0 (Array.get 0 image) 
  
 *)
(* converts a color to an rgb file *)  
 let to_rgb c =
    let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in (r,g,b)
;; 

(* helper function that compares colors and gives a cost value for the two *)
(* Lower the cost the  better *)  
let compare_colors (c1 : color) (c2 : color) : int =  
  let (r1, b1, g1) = to_rgb c1 in 
  let (r2, b2, g2) = to_rgb c2 in 
  let pre_cost = abs(r2 - r1) + abs(b2 - b1) + abs(g2 - g1) in 
  pre_cost 
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
  let n1 = Array.length caa1
  and n2 = Array.length caa2 in
  let result = Array.create (max n1 n2) caa1.(0) in
  for i = 0 to n1 - 1 do result.(i) <- (compare_c_array caa1.(i) caa2.(i)) done; 
  result;; 

let counts_array (iaa : int array array) : int = 
  let n1 = Array.length iaa in 
  let result = ref 0 in 
  for i = 0 to n1 - 1 do result := ((Array.length iaa.(i)) + !result) done;
  !result;;

(* counts the number of items in the int matrix *) 
let counts_array (iaa : int array array) : int = 
   Array.fold_right ~f:(fun x rest -> (Array.length x) + rest) iaa 0  
;; 
let average_array (iaa : int array array) : int = 


(* works on these two lists *) 
let blue_list = Array.create 2 (Array.create 2 blue) in
let red_list = Array.create 2 (Array.create 2 red) in 
compare_pixmap blue_list red_list;;
(*
let create_pixmap x y f g s = 
   let r = Array.make_matrix x y false in 
   { w = x; h = y; fg = f;  bg = g; pix = r; s = s} ;;

 *)   
(* 


1. Figure out what a bit map looks like/how it is represented. If it is just an array of arrays (Or a matrix
are the values then floats?  Could I just make a small bit map to use  to create the function
"Compare bit maps"  - so, I think I am going to assume that when we have over lapping rgb files
the two files simply average out.


2. Make a function that compares two colors and gives a mini cost for that. 
   
   - Then make the color go into the three values   - then compare the three values and gi   - First iterate through a bit mapve a number based upon the result 
 
2. Create a function that then compares two bit maps point by point, and then for every point gives a number 
from 1 - 100 (or whatever).
- Create a helper function that compares two points.

3. Create a function that then averages the all of the points - returning a cost function. 

4. Create a function that converts a guess to a bitmap 
- Createa function that stores in the width and height, a and then  
is an array of array that stores all of the data points that we want? 
- This will be interesting to represent. I'm still thinking about the best way to 
go about doing this. 



(* 
(* converts guess to an image *) 
fun make_image (g: guess) : image ->
XXX Do I want this "image" to actually just be an rmb map of rgb values? 


(* Compares guess and image and returns the relative fitness of guess *) 
fun cost (g : guess) (i : image) : int -> 
to_rgb will help me compare the rgb values
 *) 
