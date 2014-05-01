(* Bitmap.ml - implementation *)

let fourbitstring (n : int) : string =
	let l = Buffer.create 4 in
	let r = n mod 256 in
	let r1 = (n / 256) mod 256 in
	let r2 = (n / (256 * 256)) mod 256 in
	let r3 = (n / (256 * 256 * 256)) mod 256 in
	Buffer.add_char l (Char.chr r);
	Buffer.add_char l (Char.chr r1);
	Buffer.add_char l (Char.chr r2);
	Buffer.add_char l (Char.chr r3);
	Buffer.contents l
;;

let int_of_fourbitstring (s : string) : int =
	(Char.code s.[0]) + ((Char.code s.[1]) * 256) + ((Char.code s.[2]) * 256 * 256)) + ((Char.code s.[3]) * 256 * 256 * 256)
;;	

let gen_header (x : int) (y : int) : string = 
	Printf.sprintf "BM%s\000\000\000\0006\000\000\000(\000\000\000%s%s\001\000\024\000\000\000\000\000%s\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000" (fourbitstring (x * y * 3 + 54)) (fourbitstring x) (fourbitstring y) (fourbitstring (x * y * 3))
;;

let make_emptybmp (filename : string) (x : int) (y : int) (f : int -> char) : unit = 
	let channel = open_out_bin filename in
	let arr = Array.init (x * y * 3) f in
	output_string channel (gen_header x y);
	Array.iter (fun x -> output_char channel x) arr;
	close_out channel
;;

let white = make_emptybmp (fun x -> '\255');;

let operate_on_image (img1 : string) (img2 : string) (output_filename : string) (f : int -> int -> int) : unit =
	let new_channel = open_out_bin output_filename in
	let input_channel1 = open_in_bin img1 in
	let input_channel2 = open_in_bin img2 in
	seek_in input_channel1 18;
	seek_in input_channel2 18;
	let x1 = Scanf.fscanf input_channel1 "%4s" (fun x -> int_of_fourbitstring x) in 
	let y1 = Scanf.fscanf input_channel1 "%4s" (fun x -> int_of_fourbitstring x) in 
	let x2 = Scanf.fscanf input_channel2 "%4s" (fun x -> int_of_fourbitstring x) in 
	let y2 = Scanf.fscanf input_channel2 "%4s" (fun x -> int_of_fourbitstring x) in 
	if ((x1 = x2) & (y1 = y2)) then
		(output_string new_channel (gen_header x y);
			seek_in input_channel1 54;
			seek_in input_channel2 54;
			try
				while (true) do
					let c1 = input_char input_channel1 in
					let c2 = input_char input_channel2 in
						output_char new_channel (Char.chr (f (Char.code c1) (Char.code c2)))
					done 
				with End_of_file -> close_out new_channel; close_in input_channel1; close_in input_channel2)
	else
		raise (Invalid_argument "Cannot do operation on images of different sizes.")
;;

let xorimage = operate_on_image lxor;;
let landimage = operate_on_image land;;



