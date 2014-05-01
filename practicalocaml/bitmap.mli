(* Bitmap.mli - signature *)

val fourbitstring : int -> string
val int_of_fourbitstring : string -> int
val gen_header : int -> int -> string
val make_emptybmp : string -> int -> int -> (int -> char) -> unit
val operate_on_image : string -> string -> string -> (int -> int -> int) -> unit

(* made from make_emptybmp and operate_on_image
val white : string -> int -> int -> unit
val xorimage : string -> string -> string -> unit
val landimage : string -> string -> string -> unit
*)