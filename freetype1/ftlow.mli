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

(* $Id: ftlow.mli,v 1.5 2004/09/27 07:52:46 weis Exp $ *)

(** Almost all of the functions are just interpretation of functions of
  the freetype library. See the documents of the freetype library. *)

type engine;;

val init : unit -> engine;;
val close : engine -> unit;;

type face;;

type face_properties = {
  num_glyphs : int;
  num_max_points : int;
  num_max_contours : int;
  num_charmaps : int;
  num_names : int;
  num_faces : int;
  has_horizontal : bool;
  has_vertical : bool;
 };;

val open_face : engine -> string -> face;;
val close_face : face -> unit;;
(* val flush_face : face -> unit *)

type instance;;

val open_instance : face -> instance;;
val close_instance : instance -> unit;;
val set_instance_resolutions : instance -> int -> int -> unit;;
val set_instance_charsizes : instance -> int -> int -> unit;;
val set_instance_pixelsizes : instance -> int -> int -> int -> unit;;

type glyph;;

val open_glyph : face -> glyph;;
val close_glyph : glyph -> unit;;

val get_num_glyphs : face -> int;;

val load_glyph : instance -> glyph -> int -> bool -> bool -> unit;;

type outline;;
type bbox = {
  xmin : int;
  ymin : int;
  xmax : int;
  ymax : int;
 };;

(** Glyph metrics (You should use big metrics) *)
type small_glyph_metrics = {
  small_bbox : bbox;
  small_bearingx : int; (* 26.6 *)
  small_bearingy : int; (* 26.6 *)
  small_advance : int;   (* 26.6 *)
 };;

(** Big glyph metrics *)
type glyph_metrics = {
  bbox : bbox;
  hori : bearing_advance;
  vert : bearing_advance;
 }

and bearing_advance = {
    bearingx : int; (* 26.6 *)
    bearingy : int; (* 26.6 *)
    advance : int; (* 26.6 *)
 };;

type instance_metrics = {
    x_ppem : int;
    y_ppem : int;
    x_scale : int; (* 16.16 *)
    y_scale : int; (* 16.16 *)
 };;

val get_maximum_bbox : face -> bbox;;

val get_glyph_outline : glyph -> outline;;
val get_small_glyph_metrics : glyph -> small_glyph_metrics;;
val get_glyph_metrics : glyph -> glyph_metrics;;
val get_instance_metrics : instance -> instance_metrics;;
val close_outline : outline -> unit;;
val transform_outline : outline -> int -> int -> int -> int -> unit;;
val translate_outline : outline -> int -> int -> unit;;
val get_outline_bbox : outline -> bbox;;
val get_face_properties : face -> face_properties;;

type outline_tag =
   | On_point | Off_point_conic | Off_point_cubic;;

type outline_contents = {
  n_contours : int;
  n_points : int;
  points : (int * int) array;
  tags : outline_tag array; (* it is called flags in Freetype1 *)
  contours : int array;
 };;

val get_outline_contents : outline -> outline_contents;;

type map;;

val open_pixmap : int -> int -> map;;
val open_bitmap : int -> int -> map;;
val close_map : map -> unit;;
val clear_map : map -> unit;;
val get_outline_pixmap : engine -> outline -> map -> unit;;
val get_glyph_pixmap : glyph -> map -> int -> int -> unit;;
val get_outline_bitmap : engine -> outline -> map -> unit;;
val get_glyph_bitmap : glyph -> map -> int -> int -> unit;;
val read_pixmap : map -> int -> int -> int;;
val read_bitmap : map -> int -> int -> int;;

type charmap;;

val get_charmap_ids : face -> (int * (int * int)) list;;
val get_charmap_count : face -> int;;
val get_charmap_id : face -> int -> int * int;;
val get_charmap : face -> int -> charmap;;
val close_charmap : charmap -> unit;;
val char_index : charmap -> int -> int;;
