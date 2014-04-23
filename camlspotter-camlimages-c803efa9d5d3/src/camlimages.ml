(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: camlimages.ml.in,v 1.3.2.1 2010/05/13 13:14:47 furuse Exp $ *)

let version = "4.0.0";;

(* Supported libraries *)
let lib_gif = false;;
let lib_png = false;;
let lib_jpeg = false;;
let lib_tiff = false;;
let lib_freetype = false;;
let lib_ps = true;;
let lib_xpm = false;;

(* External files *)
let path_rgb_txt = "/usr/share/X11/rgb.txt";;
let path_gs = "/usr/bin/gs";;

(* They are written in ML, so always supported *)
let lib_ppm = true;;
let lib_bmp = true;;
let lib_xvthumb = true;;

(* Word size, used for the bitmap swapping memory management *)
let word_size = 4;;
