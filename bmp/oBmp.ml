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

(* $Id: oBmp.ml,v 1.7 2004/09/07 07:31:01 weis Exp $ *)

open OImages;;

let load name opts = OImages.make (Bmp.load name opts);;

let save name opts image =
  match image#image_class with
  | ClassRgb24 | ClassIndex8 -> Bmp.save name opts image#image
  | _ -> raise Wrong_image_class;;
