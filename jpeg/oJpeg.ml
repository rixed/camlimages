(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: oJpeg.ml,v 1.8 2004/09/07 07:31:05 weis Exp $ *)

open OImages;;

let load name opts = OImages.make (Jpeg.load name opts);;

let load_thumbnail name opts geom_spec =
  let w, h, img = Jpeg.load_thumbnail name opts geom_spec in
  w, h, OImages.make img;;

let save name opts image =
  let rgb24 = OImages.rgb24 image in
  Jpeg.save name opts rgb24#image;;

let save_as_cmyk name opts trans image =
  let rgb24 = OImages.rgb24 image in
  Jpeg.save_as_cmyk name opts trans rgb24#image;;


