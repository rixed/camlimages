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

(* $Id: graphic_image.mli,v 1.9 2004/09/07 07:31:04 weis Exp $ *)

(** Interface of images from this library to images of the Caml
   [Graphics] library. *)

open Images;;

val draw_image : Images.t -> int -> int -> unit;;
 (** Draw an [Images.t] value into the graphic window. *)

val get_image : int -> int -> int -> int -> Rgb24.t
 (** Capture the contents of a rectangle of the graphic window
    with lower left corner at [x,y], width [w] and height [h] *)
 
(** lower interface *)

val array_of_image : Images.t -> Graphics.color array array;;
 (** Convert an [Images.t] value to an array of colors. *)

val of_image : Images.t -> Graphics.image;;
 (** Convert an [Images.t] value to a graphic image. *)

val image_of : Graphics.image -> Rgb24.t
 (** Convert an [Graphics.image] value to *our* image *)
