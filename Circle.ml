open Core.Std
open Graphics
open Statistics
open Helpers.Helpers

(* Module that performs operations on Circles *)
module type CIRCLE =
sig
  type circle
  
  (* 'fresh a b v' returns a fresh randomly initialized circle 
   * with center within the rectangle delimited by (0,0) (a, b) *)
  val fresh : int -> int -> circle
  
  (* Returns the center of the circle *)
  val center : circle -> (float * float)
  
  (* Returns the radius of the circle *)
  val radius : circle -> float
  
  (* Makes a new circle out of a center, a radius, and a color *)
  val make : (float * float) -> float -> color -> circle
  
  (* Returns the color of the circle as an RGB triple *)
  val rgb : circle -> (int * int * int)
  
  (* Returns the color of the circle *)
  val color : circle -> color
  
  (* Returns whether circle contains point *)
  val contains : circle -> (float * float) -> bool
  
  (* 'sexual_reproduction p1 p2 c' returns a circle
   * daughter of p1 and p2 with crossing over level of c *)
  val sexual_reproduction : float -> circle -> circle -> circle
  
  (* Returns true if two circles are equal and false otherwise *)
  val equals : circle -> circle -> bool
  
  (* Prints a circle *)
  val print : circle -> unit
  
  (* Runs tests on this Module *)
  val run_tests : unit -> unit
end

module Circle : CIRCLE with type circle= (float * float) * float * color =
struct
  
  type circle = (float * float) * float * color

  let fresh a b = 
    let fa, fb = Float.of_int a, Float.of_int b in
    (random_point fa fb, Random.float (max fa fb), random_color ())

  let make (ctr : (float * float)) (r : float) (c : color) = (ctr,r,c)
  
  let center (ctr,_,_) = ctr
  
  let center_x circ = 
    let x,_ = center circ in 
    x
    
  let center_y circ = 
    let _,y = center circ in 
    y

  let radius (_,r,_) = r

  let color (_,_,col) = col

  let rgb circ = to_rgb (color circ)

  let contains circ (x,y) =
    let x', y' = center_x circ, center_y circ in
    (radius circ)**2. >= (x' -. x)**2. +. (y' -. y)**2.

  let point_reproduction std_dev p1 p2 =
    let (x,y) = halfway_point p1 p2 in
    (Statistics.gaussian_pick std_dev x, Statistics.gaussian_pick std_dev y)

  let color_reproduction std_dev c1 c2 =
    let f a1 a2 = Int.of_float (Statistics.gaussian_pick std_dev ((Float.of_int (a1 + a2)) /. 2.0)) in
    let (r1,g1,b1) = to_rgb c1
    and (r2,g2,b2) = to_rgb c2 in
    Graphics.rgb (f r1 r2) (f g1 g2) (f b1 b2)
    
  let float_reproduction std_dev r1 r2 =
    let mid_point = (r1 +. r2) /. 2. in
    Statistics.gaussian_pick std_dev mid_point  
  
  let sexual_reproduction std_dev circ1 circ2 = 
    let new_center = point_reproduction std_dev (center circ1) (center circ2) in
    let new_radius = float_reproduction std_dev (radius circ1) (radius circ2) in
    let new_color = color_reproduction std_dev (color circ1) (color circ2) in
    (new_center, new_radius, new_color)

  let test_center () =
    let p1 = make (0.,0.) 3. blue in
    let p2 = make (1.,2.) 4. red in
    let p3 = make (3.,4.) 5. green in
    assert(center p1 = (0., 0.));
    assert(center p2 = (1.,2.));
    assert(center p3 = (3.,4.))

  let test_contains () =
    let c1 = make (1.,0.) 30. blue in
    let c2 = make (10.,22.) 4. red in
    
    assert (contains c1 (30.,0.));
    assert (not (contains c1 (32., 0.)));
    assert (contains c1 (10.,10.));
    assert (not (contains c1 (30., 30.)));
    
    assert (contains c2 (14.,22.));
    assert (not (contains c2 (14., 23.)));
    assert (contains c2 (12.,24.));
    assert (not (contains c2 (10., 17.)))

  let test_color () =
    let p1 = make (0.,0.) 3. blue in
    let p2 = make (1.,2.) 4. red in
    let p3 = make (3.,4.) 5. green in
    assert(color p1 = blue);
    let r,g,b = rgb p1 in
    assert(Graphics.rgb r g b = blue);
    assert(color p2 = red);
    let r,g,b = rgb p2 in
    assert(Graphics.rgb r g b = red);
    assert(color p3 = green);
    let r,g,b = rgb p3 in
    assert(Graphics.rgb r g b = green)


  (* Prints a circle for testing purposes *)
  let print p =
    print_endline "######### Start of Circle #########";
    Printf.printf "Center: (%f, %f)\nRadius: %f\nColor: %x\n" (center_x p) (center_y p) (radius p) (color p);
    print_endline "######### End of Circle #########"

  let equals p1 p2 =
    center p1 = center p2 && radius p1 = radius p2 && color p1 = color p2

  let test_sexual_reproduction () =
    let p1 = make (0.,0.) 3. blue in
    let p2 = make (1.,2.) 4. red in
    let p3 = make (3.,4.) 5. green in
    
    let r1 = make (0.5,1.) 3.5 (halfway_color blue red) in
    let r2 = make (2.,3.) 4.5 (halfway_color red green) in
    let r3 = make (1.5,2.) 4. (halfway_color blue green) in
    
    assert(equals (sexual_reproduction 0. p1 p2) r1);
    assert(equals (sexual_reproduction 0. p2 p3) r2);
    assert(equals (sexual_reproduction 0. p1 p3) r3)

  let run_tests () =
    test_center ();
    test_color ();
    test_sexual_reproduction ();
    test_contains ();
    ()

end

