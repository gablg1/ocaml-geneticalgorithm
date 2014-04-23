open Graphics
open Array 
open Core.Std
open Images;;
open OImages;;
(* XXX Remember to open the CamlImages file
 * converts a file to an image that we can use *) 

let file = ref "hello.bmp" in
    let file = !file in
let oimage = OImages.load file [] in
let open_image_print i = 
  let rgb = Graphics.dump_image i in
  let first_pixel = Array.get (Array.get rgb 0) 0 in
  Printf.printf "%d" first_pixel in
open_image_print oimage 

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

