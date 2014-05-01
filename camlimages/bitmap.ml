type bmp = {
   bmpFileHeader : bitmapfileheader;           (* Bytes <0  14< *)
   bmpInfoHeader : bitmapinfoheader;           (* Bytes <14 54< *)
   bmpRgbQuad : rgb array;                     (* Bytes <54 ... *)
   bmpBytes : string;                          (* Bytes <bfOffBits ... *)
}

and bitmapfileheader = {
    (* WORD: that is 2 bytes *) bfType : int;  (* Bytes <0   2< *)
    (* DWORD: that is 2 WORDs *) bfSize : int; (* Bytes <2   6< *)
    (* WORD *) bfReserved1 : int;              (* Bytes <6   8< *)
    (* WORD *) bfReserved2 : int;              (* Bytes <8  10< *)
    (* DWORD *) bfOffBits : int;               (* Bytes <10 14< *)
}

and bitmapinfoheader = {
    (* DWORD *) biSize : int;                  (* Bytes <14 18< *)
    (* DWORD *) biWidth : int;                 (* Bytes <18 22< *)
    (* DWORD *) biHeight : int;                (* Bytes <22 26< *)
    (* WORD *) biPlanes  : int;                (* Bytes <26 28< *)
    (* WORD *) biBitCount : bibitcount;        (* Bytes <28 30< *)
    (* DWORD *) biCompression : bicompression; (* Bytes <30 34< *)
    (* DWORD *) biSizeImage : int;             (* Bytes <34 38< *)
    (* DWORD *) biXPelsPerMeter : int;         (* Bytes <38 42< *)
    (* DWORD *) biYPelsPerMeter : int;         (* Bytes <42 46< *)
    (* DWORD *) biClrUsed : int;               (* Bytes <46 50< *)
    (* DWORD *) biClrImportant : int           (* Bytes <50 54< *)
}
;;

let write_bmp oc = function
 { bmpFileHeader = bmpFileHeader;
   bmpInfoHeader = bmpInfoHeader;
   bmpRgbQuad = colormap;
   bmpBytes = _bitmap } as bmp ->
 bytes_written := 0;
 let start_index, bfSize_index, bfOffBits_index, end_bmpFileHeader =
   write_bmpFileHeader oc bmpFileHeader in
 let start_bmpInfoHeader = end_bmpFileHeader in
 let biSize_index, biSizeImage_index, end_bmpInfoHeader =
   write_bmpInfoHeader oc bmpInfoHeader in

 write_colors oc colormap;

 let start_bitmap_index, end_bitmap_index =
  write_image_data oc bmp in

 (* Correcting sizes: bfSize, bfOffBits, biSize, bisizeImage *)
 let bfSize = (* Given in bytes! not in DWORDs *)
       !bytes_written - start_index in
 seek_out oc bfSize_index;
 output_dword oc bfSize;

 let bfOffBits = (* Given in bytes *)
       start_bitmap_index - start_index in
 seek_out oc bfOffBits_index;
 output_dword oc bfOffBits;

 let biSize = (* Given in bytes *)
       end_bmpInfoHeader - start_bmpInfoHeader in
 seek_out oc biSize_index;
 output_dword oc biSize;

 let biSizeImage = (* Given in bytes *)
       end_bitmap_index - start_bitmap_index in
 seek_out oc biSizeImage_index;
 output_dword oc biSizeImage;
;;

let write_bmp_file fname bmp =
 let oc = open_out_bin fname in
 write_bmp oc bmp;
 close_out oc;
;;

let save_bmp = write_bmp_file
and load_bmp = read_bmp_file
;;