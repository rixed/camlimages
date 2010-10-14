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

(* $Id: japanese.ml,v 1.6 2004/09/27 07:52:47 weis Exp $ *)

open Images;;
open Rgb24;;
open Freetype;;

module FtDraw = Fttext.Make(Rgb24);;

let t = Freetype.init () in
(* You need some Japanese true type font. They are not free. *) 
let face,info = new_face t "/usr/share/fonts/TrueType/msmincho.ttc" 0 in
let cmaps = get_charmaps face in
List.iter (fun cmap ->
  prerr_endline
   (Printf.sprintf "pid=%d eid=%d" cmap.platform_id cmap.encoding_id))
  cmaps;
prerr_endline (Printf.sprintf "glyphs: %d" (get_num_glyphs face));
set_charmap face { platform_id = 3; encoding_id = 1 };
set_char_size face 36.0 36.0 72 72;

let string = "»Ò±¯ÆÒ±¬Ã¤Ì¦¸áÌ¤¿½ÆÓØü°ç" in
let string' = Fttext.unicode_of_euc_japan string in
let mapsize = 700 in
let rgb = Rgb24.create mapsize mapsize in
for x = 0 to mapsize - 1 do
  for y = 0 to mapsize - 1 do
    Rgb24.unsafe_set rgb x y {r=255; g=255; b=255}
  done
done;
let debug org level =
  let level = 255 - level in
  { r = if org.r > level then level else org.r;
    g = if org.g > level then level else org.g;
    b = if org.b > level then level else org.b; } in
set_char_size face 36.0 36.0 72 72;
FtDraw.draw_text face debug rgb 30 30 string';
FtDraw.draw_rotated_text face debug rgb 30 50 (-1.0) string';

set_char_size face 36.0 36.0 36 36;
FtDraw.draw_text face debug rgb 30 80 string';
FtDraw.draw_rotated_text face debug rgb 30 100 (-1.0) string';

Tiff.save "test.tiff" [] (Rgb24 rgb)';;

