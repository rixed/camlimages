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

(* $Id: freetypetest.ml,v 1.7 2004/09/27 07:52:47 weis Exp $ *)

open Images;;
open Rgb24;;
open Freetype;;

module FtDraw = Fttext.Make(Rgb24);;

let t = Freetype.init () in
(* you need to find some free true type font *)
let face,info = new_face t "micap.ttf" 0 in

let cmaps = get_charmaps face in
List.iter (fun cmap ->
  prerr_endline
    (Printf.sprintf "pid=%d eid=%d" cmap.platform_id cmap.encoding_id))
  cmaps;

prerr_endline (Printf.sprintf "glyphs: %d" (get_num_glyphs face));

set_char_size face 36.0 36.0 72 72;

let string = "FreeType Interface for CamlImages" in
let string = Fttext.unicode_of_latin string in

(*
let x1,y1,x2,y2 = Fttext.size face instance string in
prerr_endline (Printf.sprintf "%fx%f %fx%f" x1 y1 x2 y2);
*)
let mapsize = 700 in
let rgb = Rgb24.create mapsize mapsize in
for x = 0 to mapsize - 1 do
  for y = 0 to mapsize - 1 do
    Rgb24.unsafe_set rgb x y {r = 255; g = 255; b = 255}
  done
done;

let debug org level =
  let level = 255 - level in
  { r = if org.r > level then level else org.r;
    g = if org.g > level then level else org.g;
    b = if org.b > level then level else org.b; } in

  set_char_size face 36.0 36.0 72 72;
  FtDraw.draw_text face debug rgb 30 30 string;
prerr_endline "draw done";
  FtDraw.draw_rotated_text face debug rgb 30 50 (-1.0) string;
prerr_endline "draw_rotated done";
  FtDraw.draw_mono_text face debug rgb 30 70 string;
prerr_endline "draw_mono done";
  FtDraw.draw_mono_rotated_text face debug rgb 30 90 (-1.0) string;
prerr_endline "draw_mono_rotated done";

  set_char_size face 18.0 18.0 72 72;
  FtDraw.draw_text face debug rgb 30 120 string;
prerr_endline "draw done";
  FtDraw.draw_rotated_text face debug rgb 30 150 (-1.0) string;
prerr_endline "draw_rotated done";
  FtDraw.draw_mono_text face debug rgb 30 180 string;
prerr_endline "draw_mono done";
  FtDraw.draw_mono_rotated_text face debug rgb 30 210 (-1.0) string;
prerr_endline "draw_mono_rotated done";

  Tiff.save "test.tiff" [] (Rgb24 rgb);;
