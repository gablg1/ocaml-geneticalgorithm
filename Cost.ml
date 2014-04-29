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

(* counts the number of elements in an array array, used to average cost functions *) 
let counts_array (iaa : int array array) : int = 
  let n1 = Array.length iaa in 
  let result = ref 0 in 
  for i = 0 to n1 - 1 do result := ((Array.length iaa.(i)) + !result) done;
  !result;;

(* counts the number of items in the int matrix *) 
let counts_array (iaa : int array array) : int = 
   Array.fold_right iaa  ~f:(fun x rest -> (Array.length x) + rest) ~init:0  
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
let green_list_list = Array.create 2 (Array.create 3 green);; 
cost_of_mat blue_list_list green_list_list;;  
let array1 = Array.create 2 2 in 
assert(sum_array array1 = 4); 
let mat1 = Array.create 2 (Array.create 2 2) in 
let mat2 = Array.create 2 (Array.create 2 4) in 
assert(average_array mat1 = 2.0);; 


(* defines type triangle as a record with three points *)  
type triangle = {p1 : int * int; p2: int * int; p3 : int * int} 
let get_point1 t = t.p1;; 
let get_point2 t = t.p2;;
let get_point3 t = t.p3;; 
(* takes in two points and gives the equation between them *)  
let intersecting_line (p1 : int * int) (p2 : int * int) : float -> float = 
  let (x1, y1) = p1 in 
  let (x2, y2) = p2 in 
  fun x -> ( (float (y2 - y1)) /. (float (x2 - x1)) *. (x -. (float x1)) +. (float y1))


(* some corner cases have not been solved, i.e.  lines that are 
straight up *) 
let equation1 = intersecting_line (0,0) (3,4);;
assert (equation1 3. = 4.);;
let equation2 = intersecting_line (0,0) (0, 3);; 
(* (assert equation2 4. = inf) ;; *) 
let equation3 = intersecting_line (0,0) (3,0);; 
assert (equation3 4. = 0.);; 


(* takes in a triangle and give back the equations of the intersecting lines *) 
let triangle_lines (tr : triangle) : ('a -> 'a) * ('a -> 'a) * ('a -> 'a) =  
  let equation1 = intersecting_line (get_point1 tr) (get_point2 tr) in 
  let equation2 = intersecting_line (get_point1 tr) (get_point3 tr) in 
  let equation3 = intersecting_line (get_point2 tr) (get_point3 tr) in 
  (equation1, equation2, equation3)

(* checks to see whether a point is inside a triangle *) 
let is_in_triangle (tr : triangle) (point : int * int) : bool = 




 
(* 

 - Create an equation based upon each of those points that gives the line 
                - iF it is a concave polygon then find the three closest points 
                - Create a function that tells whether a point is bound by the points of a triangle. 
                    -    Creates a function that gives the equations of the three lines.  of each of the points of the triangle 
                    - if the point is in all three of these planes then the point is in the triangle. 
                    - Otherwise it is not in the trangle. 
                - Create an equation that can keep track of the point in the matrix. 


4. Create a function that converts a guess to a bitmap 

- Createa function that stores in the width and height, a and then  
is an array of array that stores all of the data points that we want? 
       -
       -  Figure out what are the limitations to the polygon's width and height points

       - Figure out what the equation will be between the polygons (to see whether a certain point is inside or not inside
       - Create a function that maps through the color array, (Does this color array have points?) (Are the limitations of the 
         polygon what are defined by the float? 


        1. Takes in a guess and gets out the W and H 
        2. Takes in the width and height and creates a color matrix of white 
        3. Takes in a polygon and then cuts off the float points 
        4. Uses the polygon's to form an equation that says whether or not a point would be in it 
                
                - Create an equation that takes four points and then gives the four points that would pair togethr  (I.e. those that are closest)  
                - Create an equation based upon each of those points that gives the line 
                - iF it is a concave polygon then find the three closest points 
                - Create a function that tells whether a point is bound by the points of a triangle. 
                    -    Creates a function that gives the equations of the three lines.  of each of the points of the triangle 
                    - if the point is in all three of these planes then the point is in the triangle. 
                    - Otherwise it is not in the trangle. 
                - Create an equation that can keep track of the point in the matrix. 

pairs up the points that are closes Is there a way to simplify this even more? Say between two points creates an equation. 

- This will be interesting to represent. I'm still thinking about the best way to 
go about doing this. 

- Create a function that when taking in two colors averages the two rgb coordinates 
- Create a function that takes a polygon makes it into a color matrix. 



(* 
(* converts guess to an image *) 
fun make_image (g: guess) : image ->
XXX Do I want this "image" to actually just be an rmb map of rgb values? 


(* Compares guess and image and returns the relative fitness of guess *) 
fun cost (g : guess) (i : image) : int -> 
to_rgb will help me compare the rgb values
 *) 
