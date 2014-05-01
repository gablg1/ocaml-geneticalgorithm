(** The type of bmp images. *)

(**
  The Caml representation of a bmp bit map image.
  Fields are Caml values, the decoded versions of raw data in the file.

  Structure of bitmaps files on disk :
   - BITMAPFILEHEADER    : bytes 0 to 14 excluded
   - BITMAPINFOHEADER    : bytes 14 to 54 excluded
   - RGBQUAD []          : color map
   - BYTES []            : bit map
*)
type bmp = {
   bmpFileHeader : bitmapfileheader;           (** Bytes <0  14< *)
   bmpInfoHeader : bitmapinfoheader;           (** Bytes <14 54< *)
   bmpRgbQuad : Images.rgb array;              (** Bytes <54 ... *)
   bmpBytes : string;                          (** Bytes <bfOffBits ... *)
}

and bitmapfileheader = {
    (* WORD: that is 2 bytes *) bfType : int;  (** Bytes <0   2< *)
    (* DWORD: that is 2 WORD *) bfSize : int;  (** Bytes <2   6< *)
    (* WORD *) bfReserved1 : int;              (** Bytes <6   8< *)
    (* WORD *) bfReserved2 : int;              (** Bytes <8  10< *)
    (* DWORD *) bfOffBits : int;               (** Bytes <10 14< *)
}


and bitmapinfoheader = {
    (* DWORD *) biSize : int;                  (** Bytes <14 18< *)
    (* DWORD *) biWidth : int;                 (** Bytes <18 22< *)
    (* DWORD *) biHeight : int;                (** Bytes <22 26< *)
    (* WORD *) biPlanes  : int;                (** Bytes <26 28< *)
    (* WORD *) biBitCount : bibitcount;        (** Bytes <28 30< *)
    (* DWORD *) biCompression : bicompression; (** Bytes <30 34< *)
    (* DWORD *) biSizeImage : int;             (** Bytes <34 38< *)
    (* DWORD *) biXPelsPerMeter : int;         (** Bytes <38 42< *)
    (* DWORD *) biYPelsPerMeter : int;         (** Bytes <42 46< *)
    (* DWORD *) biClrUsed : int;               (** Bytes <46 50< *)
    (* DWORD *) biClrImportant : int;          (** Bytes <50 54< *)
}
;;

val load_bmp : string -> bmp;;
val save_bmp : string -> bmp -> unit;;
 (** Load and save functions for BMP images. *)
