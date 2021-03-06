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

(* $Id: oPng.ml,v 1.8 2004/09/07 07:31:06 weis Exp $ *)

open OImages;;

let load_as_rgb24 name opts = OImages.make (Png.load_as_rgb24 name opts);;

let load name opts = OImages.make (Png.load name opts);;

let save name opts image = Png.save name opts image#image;;
