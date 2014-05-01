open Core.Std
open Graphics
open Statistics
open Helpers.Helpers

let max_radius = 50.

(* Module that performs operations on Circles *)
module type CIRCLE =
sig
  (* Raised when there is incompatibility of max width/height *)
  exception DimensionTrouble

  type circle
  
  (* 'fresh a b v' returns a fresh randomly initialized circle 
   * with center within the rectangle delimited by (0,0) (a, b) *)
  val fresh : int -> int -> circle
  
  (* Returns the center of the circle *)
  val center : circle -> (float * float)
  
  (* Returns the radius of the circle *)
  val radius : circle -> float
  
  (* Returns the maximum width of the environment *)
  val width : circle -> int
  
  (* Returns the maximum height of the environment *)
  val height : circle -> int
  
  (* Makes a new circle out of a (width, height), a center, a radius, and a color *)
  val make : (int * int) -> (float * float) -> float -> color -> circle
  
  (* Returns the color of the circle as an RGB triple *)
  val rgb : circle -> (int * int * int)
  
  (* Returns the color of the circle *)
  val color : circle -> color
  
  (* Returns whether circle contains point *)
  val contains : circle -> (float * float) -> bool
  
  (* asexual_reproduction std_dev c returns a circle daughter of c*)
  val asexual_reproduction : float -> circle -> circle
  
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

module Circle : CIRCLE =
struct
  exception DimensionTrouble
  
  type circle = (int * int) * (float * float) * float * color

  let fresh a b = 
    let fa, fb = Float.of_int a, Float.of_int b in
    ((a, b), random_point fa fb, Random.float max_radius, random_color ())

  let make dim (ctr : (float * float)) (r : float) (c : color) = (dim,ctr,r,c)
  
  let center (_,ctr,_,_) = ctr

  let center_x circ = 
    let x,_ = center circ in 
    x
    
  let center_y circ = 
    let _,y = center circ in 
    y
  
  let dimensions (d,_,_,_) = d
  
  let width circ = 
    let w,_ = dimensions circ in
    w
  
  let height circ = 
    let _,h = dimensions circ in
    h

  let radius (_,_,r,_) = r

  let color (_,_,_,col) = col

  let rgb circ = to_rgb (color circ)

  let dimensions_agree g1 g2 = (width g1 = width g2) && height g1 = height g2

  let contains circ (x,y) =
    let x', y' = center_x circ, center_y circ in
    (radius circ)**2. >= (x' -. x)**2. +. (y' -. y)**2.

  let point_reproduction std_dev (w,h) (x,y) =
    let dev = std_dev *. (Float.of_int w) in
    let new_x = Statistics.gaussian_pick dev x in
    let new_y = Statistics.gaussian_pick dev y in
    
    (* Only updates point if it's valid, otherwise keeps halfway point *)
    if valid_point (w,h) (new_x,new_y) then (new_x,new_y)
    else (x,y)

  let color_reproduction std_dev c1 =
    let f a = abs(Int.of_float (Statistics.gaussian_pick (std_dev *. 255.) (Float.of_int a))) in
    let (r1,g1,b1) = to_rgb c1 in
    Graphics.rgb (f r1) (f g1) (f b1)
    
  let pos_float_reproduction std_dev r1 =
    let dev = std_dev *. max_radius in
    let new_f = Float.abs(Statistics.gaussian_pick dev r1) in
    if new_f > max_radius then r1 else new_f
  
  let asexual_reproduction std_dev circ1 =
    let new_center = point_reproduction (std_dev) (dimensions circ1) (center circ1) in
    let new_radius = pos_float_reproduction std_dev (radius circ1) in
    let new_color = color_reproduction std_dev (color circ1) in
    (dimensions circ1,new_center, new_radius, new_color)
  
  let sexual_reproduction std_dev circ1 circ2 = 
    if not (dimensions_agree circ1 circ2) then raise DimensionTrouble else
    
    let new_center = halfway_point (center circ1) (center circ2) in
    let new_radius = ((radius circ1) +. (radius circ2)) /. 2. in
    let new_color =  halfway_color (color circ1) (color circ2) in
    asexual_reproduction std_dev (dimensions circ1,new_center, new_radius, new_color)

  let test_center () =
    let p1 = make (100.,100.) (0.,0.) 3. blue in
    let p2 = make (100.,100.) (1.,2.) 4. red in
    let p3 = make (100.,100.) (3.,4.) 5. green in
    assert(center p1 = (0., 0.));
    assert(center p2 = (1.,2.));
    assert(center p3 = (3.,4.))

  let test_contains () =
    let c1 = make (100.,100.) (1.,0.) 30. blue in
    let c2 = make (100.,100.) (10.,22.) 4. red in
    
    assert (contains c1 (30.,0.));
    assert (not (contains c1 (32., 0.)));
    assert (contains c1 (10.,10.));
    assert (not (contains c1 (30., 30.)));
    
    assert (contains c2 (14.,22.));
    assert (not (contains c2 (14., 23.)));
    assert (contains c2 (12.,24.));
    assert (not (contains c2 (10., 17.)))

  let test_color () =
    let p1 = make (100.,100.) (0.,0.) 3. blue in
    let p2 = make (100.,100.) (1.,2.) 4. red in
    let p3 = make (100.,100.) (3.,4.) 5. green in
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
    dimensions_agree p1 p2 &&
    center p1 = center p2 && radius p1 = radius p2 && color p1 = color p2

  let test_sexual_reproduction () =
    let p1 = make (100,100) (0.,0.) 3. blue in
    let p2 = make (100,100) (1.,2.) 4. red in
    let p3 = make (100,100) (3.,4.) 5. green in
    
    let r1 = make (100,100) (0.5,1.) 3.5 (halfway_color blue red) in
    let r2 = make (100,100) (2.,3.) 4.5 (halfway_color red green) in
    let r3 = make (100,100) (1.5,2.) 4. (halfway_color blue green) in
    
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

