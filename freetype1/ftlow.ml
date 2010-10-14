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

(* $Id: ftlow.ml,v 1.4 2004/09/27 07:52:46 weis Exp $ *)

(* The lower interface *)

type engine;;

external init : unit -> engine
    = "init_FreeType";;

external close : engine -> unit
    = "done_FreeType";;

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

external open_face : engine -> string -> face
    = "open_Face";;
external close_face : face -> unit
    = "close_Face";;

type instance;;

external open_instance : face -> instance
    = "new_Instance";;
external close_instance : instance -> unit
    = "done_Instance";;
external set_instance_resolutions : instance -> int -> int -> unit
    = "set_Instance_Resolutions";;
external set_instance_charsizes : instance ->
  int (* 26.6 *) -> int (* 26.6 *)-> unit
    = "set_Instance_CharSizes";;
external set_instance_pixelsizes : instance ->
  int -> int -> int (* 26.6 *) -> unit
    = "set_Instance_PixelSizes";;

type glyph;;

external open_glyph : face -> glyph
    = "new_Glyph";;
external close_glyph : glyph -> unit
    = "done_Glyph";;

external load_glyph : instance -> glyph -> int -> bool -> bool -> unit
    = "load_Glyph";;

type outline
type bbox = {
    xmin : int; (* 26.6 *)
    ymin : int; (* 26.6 *)
    xmax : int; (* 26.6 *)
    ymax : int; (* 26.6 *)
  };;

(* glyph metrics (You should use big metrics) *)
type small_glyph_metrics = {
    small_bbox : bbox;
    small_bearingx : int; (* 26.6 *)
    small_bearingy : int; (* 26.6 *)
    small_advance : int;   (* 26.6 *)
  };;

(* big glyph metrics *)
type bearing_advance = {
    bearingx : int; (* 26.6 *)
    bearingy : int; (* 26.6 *)
    advance : int; (* 26.6 *)
  };;

type glyph_metrics = {
    bbox : bbox;
    hori : bearing_advance;
    vert : bearing_advance;
  };;

(*
type instance_metrics = {
    pointsize : int;
    x_ppem : int;
    y_ppem : int;
    x_scale : int; (* 16.16 *)
    y_scale : int; (* 16.16 *)
    x_resolution : int;
    y_resolution : int;
  };;
*)
type instance_metrics = {
    x_ppem : int;
    y_ppem : int;
    x_scale : int; (* 16.16 *)
    y_scale : int; (* 16.16 *)
  };;

external get_maximum_bbox : face -> bbox
    = "get_Maximum_Bbox";;

external get_glyph_outline : glyph -> outline
    = "get_Glyph_Outline";;
external get_small_glyph_metrics : glyph -> small_glyph_metrics
    = "get_Glyph_Metrics";;
external get_glyph_metrics : glyph -> glyph_metrics
    = "get_Big_Glyph_Metrics";;
external get_instance_metrics : instance -> instance_metrics
    = "get_Instance_Metrics";;
external close_outline : outline -> unit
    = "done_Outline";;
external transform_outline : outline -> int -> int -> int -> int -> unit
    = "transform_Outline";;
external translate_outline : outline -> int -> int -> unit
    = "translate_Outline";;
external get_outline_bbox : outline -> bbox
    = "get_Outline_BBox";;
external get_face_properties : face -> face_properties
    = "get_Face_Properties";;

type outline_tag = On_point | Off_point_conic | Off_point_cubic;;

type outline_contents = {
    n_contours : int;
    n_points : int;
    points : (int * int) array;
    tags : outline_tag array; (* it is called flags in Freetype1 *)
    contours : int array;
  };;

external get_outline_contents : outline -> outline_contents
	= "get_Outline_Contents";;

(*
type pixmap_c
type pixmap = { bitmap : string; rows : int; width : int; cols : int }

external open_pixmap : int -> int -> pixmap_c
    = "new_Pixmap"
external close_pixmap : pixmap_c -> unit
    = "done_Pixmap"
external clear_pixmap : pixmap_c -> unit
    = "clear_Pixmap"
external return_pixmap : pixmap_c -> pixmap
    = "return_Pixmap"
external get_outline_pixmap : engine -> outline -> pixmap_c -> unit
    = "get_Outline_Pixmap"
external get_glyph_pixmap : glyph -> pixmap_c -> int -> int -> unit
    = "get_Glyph_Pixmap"
*)
type map;;

external open_pixmap : int -> int -> map
    = "new_Pixmap";;
external open_bitmap : int -> int -> map
    = "new_Bitmap";;
external close_map : map -> unit
    = "done_Map";;
external clear_map : map -> unit
    = "clear_Map";;
external get_outline_pixmap : engine -> outline -> map -> unit
    = "get_Outline_Pixmap";;
external get_glyph_pixmap : glyph -> map -> int -> int -> unit
    = "get_Glyph_Pixmap";;
external get_outline_bitmap : engine -> outline -> map -> unit
    = "get_Outline_Bitmap";;
external get_glyph_bitmap : glyph -> map -> int -> int -> unit
    = "get_Glyph_Bitmap";;
external read_pixmap : map -> int -> int -> int
    = "get_pixmap_pixel";;
external read_bitmap : map -> int -> int -> int
    = "get_bitmap_pixel";;

type charmap;;
external get_charmap_count : face -> int
    = "get_CharMap_Count";;
external get_charmap_id : face -> int -> int * int
    = "get_CharMap_ID";;
external get_charmap : face -> int -> charmap
    = "get_CharMap";;
external close_charmap : charmap -> unit
    = "done_CharMap";;
external char_index : charmap -> int -> int
    = "char_Index";;

external get_num_glyphs : face -> int
    = "get_num_Glyphs";;

(*
external charmap_first : charmap -> int
    = "charMap_First"
external charmap_next : charmap -> int -> int
    = "charMap_Next"
external charmap_last : charmap -> int
    = "charMap_Last"
*)

let get_charmap_ids face =
  let charmaps = ref [] in
  let n = get_charmap_count face in
  for i = 0 to n - 1 do
    charmaps := (i, get_charmap_id face i) : : !charmaps
  done;
  List.rev !charmaps;;
