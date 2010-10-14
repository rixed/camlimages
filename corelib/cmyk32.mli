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

(* $Id: cmyk32.mli,v 1.6 2004/09/24 14:27:25 weis Exp $*)
 
(* CMYK 32 bit depth image format *)

type elt = Color.cmyk;;

type rawimage;;

(* The image type *)
type t = {
  width : int; 
  height : int;
  rawimage : rawimage;
  mutable infos : Info.info list;
 };;

(* Generic functions *)
(* Please read the comments of IMAGE in genimage.mli *)

val dump : t -> string;;
val unsafe_access : t -> int -> int -> string * int;;
val get_strip : t -> int -> int -> int -> string;;
val set_strip : t -> int -> int -> int -> string -> unit;;
val get_scanline : t -> int -> string;;
val set_scanline : t -> int -> string -> unit;;
val unsafe_get : t -> int -> int -> elt;;
val unsafe_set : t -> int -> int -> elt -> unit;;
val get : t -> int -> int -> elt;;
val set : t -> int -> int -> elt -> unit;;
val destroy : t -> unit;;
val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit;;
val map : (elt -> elt -> elt) ->
  t -> int -> int -> t -> int -> int -> int -> int -> unit;;
val create_with : int -> int -> Info.info list -> string -> t;;
val create : int -> int -> t;;
val make : int -> int -> elt -> t;;
val copy : t -> t;;
val sub : t -> int -> int -> int -> int -> t;;
val resize : (float -> unit) option -> t -> int -> int -> t;;
