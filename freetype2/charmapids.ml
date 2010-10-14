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

(* $Id: charmapids.ml,v 1.4 2004/09/27 07:52:47 weis Exp $ *)

open Printf;;
open Freetype;;

let font = ref "" in
Arg.parse [] (fun x -> font := x) "charmapids font";
if !font = "" then raise Not_found;
let engine = Freetype.init () in
let face = open_face engine !font in
let charmap_ids = get_charmap_ids face in
List.iter (fun (n,(id,enc)) ->
  prerr_endline (sprintf "#%d: %d, %d" n id enc))
  charmap_ids;
close_face face;;
