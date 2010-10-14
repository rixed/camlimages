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

(* $Id: region.ml,v 1.7 2004/09/07 07:31:02 weis Exp $ *)

let error = ref (fun () -> raise (Failure "Region error"));;
  (* This will be overridden in images.ml,
     so that it raises Images.Out_of_image *)

let check width height x y =
  if x < 0 || x >= width || y < 0 || y >= height then !error ();;

