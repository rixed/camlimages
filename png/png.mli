(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Fran�ois Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: png.mli,v 1.10 2004/09/07 07:31:06 weis Exp $ *)

val check_header : string -> Images.header;;
  (** Checks the file header *)

val load : string -> Images.load_option list -> Images.t;;
  (** Loads a png image. *)

val load_as_rgb24 : string -> Images.load_option list -> Images.t;;
  (** Loads a png image, and coerce it to Rgb24. *)

val save : string -> Images.save_option list -> Images.t -> unit;;
  (** Saves an image into a png file format. *)
