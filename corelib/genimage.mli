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

(* $Id: genimage.mli,v 1.4 2004/09/07 07:31:02 weis Exp $ *)

(** color to string encoder/decoder module type *)
module type ENCODE =
  sig
    type t
    val bytes_per_pixel : int
    val get : string -> int -> t
    val set : string -> int -> t -> unit
    val make : t -> string
  end;;

(** low level image module type *)
module type RAWIMAGE = sig
  module Encode : ENCODE
  type elt
  and bitmap
  and t = { width : int; height : int; bitmap : bitmap; }
  val width : t -> int
  val height : t -> int
  val dump : t -> string
  val create_with : int -> int -> string -> t
  val create : int -> int -> t
  val make : int -> int -> elt -> t
  val unsafe_access : t -> int -> int -> string * int
  val get_strip : t -> int -> int -> int -> string
  val set_strip : t -> int -> int -> int -> string -> unit
  val get_scanline : t -> int -> string
  val set_scanline : t -> int -> string -> unit
  val unsafe_get : t -> int -> int -> elt
  val unsafe_set : t -> int -> int -> elt -> unit
  val get : t -> int -> int -> elt
  val set : t -> int -> int -> elt -> unit
  val destroy : t -> unit
  val copy : t -> t
  val sub : t -> int -> int -> int -> int -> t
  val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
  val map : (elt -> elt -> elt) ->
    t -> int -> int -> t -> int -> int -> int -> int -> unit
end;;

(** low level image creater *)
module MakeRawImage : functor (E : ENCODE) -> (RAWIMAGE with type elt = E.t);;

(** low image container module type *)
module type CONTAINER = sig
  type container
  type rawimage
  val rawimage : container -> rawimage
  val create_default : int -> int -> rawimage -> container
  val create_duplicate : container -> int -> int -> rawimage -> container
end;;

module type IMAGE = sig
  type t
  type elt

  (** Image creation *)

  val create : int -> int -> t
  (** [create w h] creates an image with a size [w]x[h]. The content is
     the image is not initialized. *)

  val make : int -> int -> elt -> t
  (** [make w h c] creates an image with a size [w]x[h]. The content is
     the image is initialized to the color [c]. *)

  val destroy : t -> unit
  (** [destroy t] explicitly frees the image content of [t].
     If you do not use bitmap swap files, you do not need to call
     this function, since GC will free unreachable image data automatically.
     Read bitmap.mli for more details. *)

  (** Pixel access *)

  val get : t -> int -> int -> elt
  (** [get t x y] gets image pixel of [t] at [x],[y]. If [x],[y] is
     outside of the image size, Images.Out_of_image exception is raised. *)

  val set : t -> int -> int -> elt -> unit
  (** [set t x y c] sets image pixel of [t] at [x],[y] by the color [c].
     If [x],[y] is outside of the image size, Images.Out_of_image exception
     is raised. *)

  val unsafe_get : t -> int -> int -> elt
  val unsafe_set : t -> int -> int -> elt -> unit
  (** Unsafe versions of [get] and [set]. It does not perform any image
     boundary check. If the coordinates are out of the given image,
     the result is undefined. Use carefully. *)

  (** Image copy *)

  val copy : t -> t
  (** [copy t] duplicates the image [t]. *)

  val sub : t -> int -> int -> int -> int -> t
  (** [sub t x y w h] duplicates a subimage of [t] of size [w]x[h],
     whose origin (0,0) is at (x,y) of [t]. *)

  val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
  (** [blit src x y dst x' y' w h] copies rectangular area of [src] at
     [x],[y] with size [w]x[h], to an image [dst]. The origin of
     the subimage comes at [x'],[y'] of [dst]. *)

  val map : (elt -> elt -> elt) ->
    t -> int -> int -> t -> int -> int -> int -> int -> unit
  (** [map f src x y dst x' y' w h] maps pixels of the rectangular area
     of [src] at  [x],[y] with size [w]x[h], to an image [dst],
     using color conversion function  [f]. The origin of the subimage
     comes at [x'],[y'] of [dst]. *)

  (** functions for internal use *)
  val dump : t -> string
  val unsafe_access : t -> int -> int -> string * int
  val get_strip : t -> int -> int -> int -> string
  val set_strip : t -> int -> int -> int -> string -> unit
  val get_scanline : t -> int -> string
  val set_scanline : t -> int -> string -> unit
end;;

module Make : functor (RI : RAWIMAGE) ->
              functor (CON : CONTAINER with type rawimage = RI.t) ->
  IMAGE with type t = CON.container
	and  type elt = RI.elt;;

module type CONTAINER_INDEXED = sig
  type container
  type rawimage
  type mapelt
  val rawimage : container -> rawimage
  val create_default : int -> int -> rawimage -> container
  val create_duplicate : container -> int -> int -> rawimage -> container
  val colormap : container -> mapelt Color.map
end;;

module type IMAGEINDEXED = sig
  type t
  type elt
  type mapelt

  (** Image creation *)

  val create : int -> int -> t
  (** [create w h] creates an image with a size [w]x[h]. The content is
     the image is not initialized. *)

  val make : int -> int -> elt -> t
  (** [make w h c] creates an image with a size [w]x[h]. The content is
     the image is initialized to the color [c]. *)

  val destroy : t -> unit
  (** [destroy t] explicitly frees the image content of [t].
     If you do not use bitmap swap files, you do not need to call
     this function, since GC will free unreachable image data automatically.
     Read bitmap.mli for more details. *)

  (** Pixel access *)

  val get : t -> int -> int -> elt
  (** [get t x y] gets image pixel of [t] at [x],[y]. If [x],[y] is
     outside of the image size, Images.Out_of_image exception is raised. *)

  val get_color : t -> int -> int -> mapelt
  (** [get_color x y] returns image pixel color value of [t] at [x],[y].
     If [x],[y] is outside of the image size, Images.Out_of_image exception
     is raised. *)

  val set : t -> int -> int -> elt -> unit
  (** [set t x y c] sets image pixel of [t] at [x],[y] by the color [c].
     If [x],[y] is outside of the image size, Images.Out_of_image exception
     is raised. *)

  val unsafe_get : t -> int -> int -> elt
  val unsafe_get_color : t -> int -> int -> mapelt
  val unsafe_set : t -> int -> int -> elt -> unit
  (** Unsafe versions of [get] and [set]. It does not perform any image
     boundary check. If the coordinates are out of the given image,
     the result is undefined. Use carefully. *)

  (** Image copy *)
  val copy : t -> t
  (** [copy t] duplicates the image [t]. *)

  val sub : t -> int -> int -> int -> int -> t
  (** [sub t x y w h] duplicates a subimage of [t] of size [w]x[h],
     whose origin (0,0) is at (x,y) of [t]. *)

  val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
  (** [blit src x y dst x' y' w h] copies rectangular area of [src] at
     [x],[y] with size [w]x[h], to an image [dst]. The origin of
     the subimage comes at [x'],[y'] of [dst]. *)

  val map : (elt -> elt -> elt) ->
    t -> int -> int -> t -> int -> int -> int -> int -> unit
  (** [map f src x y dst x' y' w h] maps pixels of the rectangular area
     of [src] at  [x],[y] with size [w]x[h], to an image [dst],
     using color conversion function  [f]. The origin of the subimage
     comes at [x'],[y'] of [dst]. *)

  (** functions for internal use *)
  val dump : t -> string
  val unsafe_access : t -> int -> int -> string * int
  val get_strip : t -> int -> int -> int -> string
  val set_strip : t -> int -> int -> int -> string -> unit
  val get_scanline : t -> int -> string
  val set_scanline : t -> int -> string -> unit
end;;

module MakeIndexed(RI:RAWIMAGE with type elt = int)
    (CON:CONTAINER_INDEXED with type rawimage = RI.t) :
  IMAGEINDEXED with type t = CON.container
               and  type elt = int
	       and  type mapelt = CON.mapelt;;

