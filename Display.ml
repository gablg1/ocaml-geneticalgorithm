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

;;

(* testing *)
;;

(* Writing img to a file *)
let save_img_file matrix i =
  let file = "iteration-" ^ (string_of_int i) ^ ".ml" in
  let num_message = (string_of_int i) in
  let message = 
    let f = Array.fold_right ~f:(fun x y -> (string_of_int x) ^ " " ^ y) ~init:"" in
    Array.fold_right matrix ~init:"" ~f:(fun x y -> (f x) ^ "\n" ^ y)
  in
  let oc = open_out file in   
  fprintf oc "%s\n%s" num_message message; close_out_noerr oc
;;   

(* testing *)
let color1 = Graphics.rgb 100 150 300 in
let m = Array.make_matrix 200 300 color1 in           
save_img_file m 4
;;  

(* Reading a file int by int, color by color *)
type reader = {read_next : unit -> string option};;

let make_reader file_name =
  let in_channel = open_in file_name in
  let closed = ref false in
  let read_next_line = fun () ->
    if !closed then
      None
    else
      try
        Some (Scanf.fscanf in_channel "%d" (fun x -> x))
      with
        End_of_file ->
          let _ = close_in_noerr in_channel in
          let _ = closed := true in
          None in
  {read_next = read_next_line};;

let r = make_reader "input.txt";;
let next_line = r.read_next ();;

let show_img_file file =
  let ic = open_in file in
  let line = input_line ic in  
  print_endline line; flush stdout; close_in_noerr ic;
  (* let i = first line, then skip the /n, then make a matrix named m
let m = (make from matrices?), and finally: 
Graphics.open_graph " 200x400"; draw_from_colors m i*)
;;       

(* testing *)
show_img_file "iteration-4.ml"
;;         

