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

(* $Id: genimage.ml,v 1.5 2004/09/07 07:31:01 weis Exp $ *)

open Color;;
open Bitmap;;

module type ENCODE = sig
  type t
  val bytes_per_pixel : int
  val get : string -> int -> t
  val set : string -> int -> t -> unit
  val make : t -> string
end;;

module type RAWIMAGE = sig
  module Encode : ENCODE
  type elt
  type bitmap

  type t = {
      width: int;
      height: int;
      bitmap: bitmap;
    }

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

module MakeRawImage(E:ENCODE) = struct
  module Bitmap= Bitmap.Make(E)
  module Encode = E

  type elt = E.t
  type bitmap = Bitmap.t

  type t = {
      width: int;
      height: int;
      bitmap: bitmap;
    }

  let width t = t.width
  let height t = t.height
  let dump t = Bitmap.dump t.bitmap

  let create_with width height init_buffer =
    { width= width;
      height= height;
      bitmap= Bitmap.create_with width height init_buffer;
    }
  ;;

  let create width height =
    { width= width;
      height= height;
      bitmap= Bitmap.create width height None;
    }
  ;;

  let make width height init =
    { width= width;
      height= height;
      bitmap= Bitmap.create width height (Some (E.make init));
    }
  ;;

  let unsafe_access t x y = Bitmap.access t.bitmap x y;;
  let get_strip t = Bitmap.get_strip t.bitmap
  let set_strip t = Bitmap.set_strip t.bitmap
  let get_scanline t = Bitmap.get_scanline t.bitmap
  let set_scanline t = Bitmap.set_scanline t.bitmap

  let unsafe_get t x y =
    let str, pos = Bitmap.access t.bitmap x y in
    E.get str pos
  ;;

  let unsafe_set t x y c =
    let str, pos = Bitmap.access t.bitmap x y in
    E.set str pos c
  ;;

  let get t x y =
    Region.check t.width t.height x y;
    unsafe_get t x y
  ;;

  let set t x y c =
    Region.check t.width t.height x y;
    unsafe_set t x y c
  ;;

  let destroy t =
    Bitmap.destroy t.bitmap
  ;;

  let copy src = { src with bitmap = Bitmap.copy src.bitmap }

  let sub src x y w h =
    { width= w;
      height= h;
      bitmap= Bitmap.sub src.bitmap x y w h;
    }
  ;;

  let blit src sx sy dst dx dy w h =
    Bitmap.blit src.bitmap sx sy dst.bitmap dx dy w h
  ;;

  let map f src sx sy dst dx dy w h =
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
	let s = unsafe_get src (sx + x) (sy + y) in
	let dx' = dx + x and dy' = dy + y in
	let d = unsafe_get dst dx' dy' in
	unsafe_set dst dx' dy' (f s d)
      done
    done
end;;

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

  (* Image creation *)

  val create : int -> int -> t
  (* [create w h] creates an image with a size [w]x[h]. The content is
     the image is not initialized. *)

  val make : int -> int -> elt -> t
  (* [make w h c] creates an image with a size [w]x[h]. The content is
     the image is initialized to the color [c]. *)

  val destroy : t -> unit
  (* [destroy t] explicitly frees the image content of [t].
     If you do not use bitmap swap files, you do not need to call
     this function, since GC will free unreachable image data automatically.
     Read bitmap.mli for more details. *)

  (* Pixel access *)

  val get : t -> int -> int -> elt
  (* [get t x y] gets image pixel of [t] at [x],[y]. If [x],[y] is
     outside of the image size, Images.Out_of_image exception is raised. *)

  val set : t -> int -> int -> elt -> unit
  (* [set t x y c] sets image pixel of [t] at [x],[y] by the color [c].
     If [x],[y] is outside of the image size, Images.Out_of_image exception
     is raised. *)

  val unsafe_get : t -> int -> int -> elt
  val unsafe_set : t -> int -> int -> elt -> unit
  (* Unsafe versions of [get] and [set]. It does not perform any image
     boundary check. If the coordinates are out of the given image,
     the result is undefined. Use carefully. *)

  (* Image copy *)

  val copy : t -> t
  (* [copy t] duplicates the image [t]. *)

  val sub : t -> int -> int -> int -> int -> t
  (* [sub t x y w h] duplicates a subimage of [t] of size [w]x[h],
     whose origin (0,0) is at (x,y) of [t]. *)

  val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
  (* [blit src x y dst x' y' w h] copies rectangular area of [src] at
     [x],[y] with size [w]x[h], to an image [dst]. The origin of
     the subimage comes at [x'],[y'] of [dst]. *)

  val map : (elt -> elt -> elt) ->
    t -> int -> int -> t -> int -> int -> int -> int -> unit
  (* [map f src x y dst x' y' w h] maps pixels of the rectangular area
     of [src] at  [x],[y] with size [w]x[h], to an image [dst],
     using color conversion function  [f]. The origin of the subimage
     comes at [x'],[y'] of [dst]. *)

  (* functions for internal use *)
  val dump : t -> string
  val unsafe_access : t -> int -> int -> string * int
  val get_strip : t -> int -> int -> int -> string
  val set_strip : t -> int -> int -> int -> string -> unit
  val get_scanline : t -> int -> string
  val set_scanline : t -> int -> string -> unit
end;;

module Make(RI:RAWIMAGE)(CON:CONTAINER with type rawimage = RI.t) = struct
  type t = CON.container
  type elt = RI.elt

  let dump t = RI.dump (CON.rawimage t)

  let create width height =
    CON.create_default width height (RI.create width height)

  let make width height c =
    CON.create_default width height (RI.make width height c)

  let unsafe_access t = RI.unsafe_access (CON.rawimage t)
  let get_strip t = RI.get_strip (CON.rawimage t)
  let set_strip t = RI.set_strip (CON.rawimage t)
  let get_scanline t = RI.get_scanline (CON.rawimage t)
  let set_scanline t = RI.set_scanline (CON.rawimage t)

  let unsafe_get t = RI.unsafe_get (CON.rawimage t)
  let unsafe_set t = RI.unsafe_set (CON.rawimage t)
  let get t = RI.get (CON.rawimage t)
  let set t = RI.set (CON.rawimage t)
  let destroy t = RI.destroy (CON.rawimage t)
  let copy t =
    let t' = CON.rawimage t in
    CON.create_duplicate t (RI.width t') (RI.height t') (RI.copy t')
  let sub t x y w h =
    let t' = CON.rawimage t in
    CON.create_duplicate t w h (RI.sub t' x y w h)
  let blit src sx sy dst dx dy w h =
    RI.blit (CON.rawimage src) sx sy (CON.rawimage dst) dx dy w h
  let map f src sx sy dst dx dy w h =
    RI.map f (CON.rawimage src) sx sy (CON.rawimage dst) dx dy w h
end;;

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

  (* Image creation *)

  val create : int -> int -> t
  (* [create w h] creates an image with a size [w]x[h]. The content is
     the image is not initialized. *)

  val make : int -> int -> elt -> t
  (* [make w h c] creates an image with a size [w]x[h]. The content is
     the image is initialized to the color [c]. *)

  val destroy : t -> unit
  (* [destroy t] explicitly frees the image content of [t].
     If you do not use bitmap swap files, you do not need to call
     this function, since GC will free unreachable image data automatically.
     Read bitmap.mli for more details. *)

  (* Pixel access *)

  val get : t -> int -> int -> elt
  (* [get t x y] gets image pixel of [t] at [x],[y]. If [x],[y] is
     outside of the image size, Images.Out_of_image exception is raised. *)

  val get_color : t -> int -> int -> mapelt
  (* [get_color x y] returns image pixel color value of [t] at [x],[y].
     If [x],[y] is outside of the image size, Images.Out_of_image exception
     is raised. *)

  val set : t -> int -> int -> elt -> unit
  (* [set t x y c] sets image pixel of [t] at [x],[y] by the color [c].
     If [x],[y] is outside of the image size, Images.Out_of_image exception
     is raised. *)

  val unsafe_get : t -> int -> int -> elt
  val unsafe_get_color : t -> int -> int -> mapelt
  val unsafe_set : t -> int -> int -> elt -> unit
  (* Unsafe versions of [get] and [set]. It does not perform any image
     boundary check. If the coordinates are out of the given image,
     the result is undefined. Use carefully. *)

  (* Image copy *)
  val copy : t -> t
  (* [copy t] duplicates the image [t]. *)

  val sub : t -> int -> int -> int -> int -> t
  (* [sub t x y w h] duplicates a subimage of [t] of size [w]x[h],
     whose origin (0,0) is at (x,y) of [t]. *)

  val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
  (* [blit src x y dst x' y' w h] copies rectangular area of [src] at
     [x],[y] with size [w]x[h], to an image [dst]. The origin of
     the subimage comes at [x'],[y'] of [dst]. *)

  val map : (elt -> elt -> elt) ->
    t -> int -> int -> t -> int -> int -> int -> int -> unit
  (* [map f src x y dst x' y' w h] maps pixels of the rectangular area
     of [src] at  [x],[y] with size [w]x[h], to an image [dst],
     using color conversion function  [f]. The origin of the subimage
     comes at [x'],[y'] of [dst]. *)

  (* functions for internal use *)
  val dump : t -> string
  val unsafe_access : t -> int -> int -> string * int
  val get_strip : t -> int -> int -> int -> string
  val set_strip : t -> int -> int -> int -> string -> unit
  val get_scanline : t -> int -> string
  val set_scanline : t -> int -> string -> unit
end;;

module MakeIndexed(RI:RAWIMAGE with type elt = int)
    (CON:CONTAINER_INDEXED with type rawimage = RI.t) = struct
      include Make(RI)(CON)
      type mapelt = CON.mapelt
      let unsafe_get_color t =
	let colormap = CON.colormap t in
	fun x y -> colormap.map.(unsafe_get t x y)
      let get_color t =
	let colormap = CON.colormap t in
	fun x y ->
	  let i = get t x y in
	  if i < 0 || i >= Array.length colormap.map then
	    raise Not_found;
	  colormap.map.(i)
end;;
