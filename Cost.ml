#load "graphics.cma"

open Graphics
open Array 
open Core.Std
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
(* 
(* converts guess to an image *) 
fun make_image (g: guess) : image -> 

(* Compares guess and image and returns the relative fitness of guess *) 
fun cost (g : guess) (i : image) : int -> 
 *) 
