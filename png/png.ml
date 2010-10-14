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

(* $Id: png.ml,v 1.11 2004/09/07 07:31:06 weis Exp $ *)

open Images;;

type png_read_result =
  | PNG_RGB24 of string
  | PNG_INDEX8 of string * rgb array
  | PNG_INDEX16 of string * rgb array
  | PNG_INDEX4 of string * rgb array;;

external read_as_rgb24 : string -> int * int * string
    = "read_png_file_as_rgb24";;
external read : string -> int * int * png_read_result
    = "read_png_file";;
external write_rgb24 : string -> string -> int -> int -> unit
    = "write_png_file_rgb24";;
external write_index : string -> string -> rgb array -> int -> int -> unit
    = "write_png_file_index";;

let load_as_rgb24 name opts =
  let w, h, buf = read_as_rgb24 name in
  Rgb24 (Rgb24.create_with w h [] buf);;

let load name opts =
  let w, h, res = read name in
  match res with
  | PNG_RGB24 buf -> Rgb24 (Rgb24.create_with w h [] buf)
  | PNG_INDEX8 (buf,cmap) ->
      Index8 (Index8.create_with w h [] { max = 255; map = cmap; } (-1) buf)
  | PNG_INDEX16 (buf,cmap) ->
      Index16 (Index16.create_with w h [] { max = 65535; map = cmap } (-1) buf)
  | PNG_INDEX4 (buf,cmap) ->
      let buf' = String.create (w * h) in
      for y = 0 to h - 1 do
	for x = 0 to w - 1 do
	  buf'.[x + y * w] <-
	    char_of_int
	      (let c = int_of_char buf.[y * ((w + 1) / 2) + x / 2] in
               if x mod 2 = 0 then c lsr 4 else c mod 16)
	done
      done;
      Index8 (Index8.create_with w h [] { max = 16; map = cmap } (-1) buf');;

let save name opts image =
  match image with
  | Rgb24 bmp ->
      write_rgb24 name (Rgb24.dump bmp) bmp.Rgb24.width bmp.Rgb24.height
  | Index8 bmp ->
      write_index name (Index8.dump bmp) bmp.Index8.colormap.map
	bmp.Index8.width bmp.Index8.height
  | Index16 bmp ->
      write_index name (Index16.dump bmp)
	bmp.Index16.colormap.map
	bmp.Index16.width bmp.Index16.height
  | Rgba32 _ | Cmyk32 _ -> failwith "Saving of RGBA and CMYK not supported";;

let check_header filename =
  let len = 24 in
  let ic = open_in_bin filename in
  try
    let str = String.create len in
    really_input ic str 0 len;
    close_in ic;
    if String.sub str 1 3 = "PNG" then begin
      if str <> "\137PNG\013\010\026\010" then
  	{ header_width= -1;
  	  header_height= -1;
  	  header_infos= [Info_Corrupted]; }
      else begin
  	let belong str =
  	  int_of_char str.[0] lsl 48 +
  	  int_of_char str.[1] lsl 32 +
  	  int_of_char str.[2] lsl 16 +
  	  int_of_char str.[3] in
  	let w = belong (String.sub str 8 4) in
  	let h = belong (String.sub str 12 4) in
  	let bdepth = Info_Depth (int_of_char str.[12]) in
  	let infos =
  	  try
  	    let colormodel =
  	      match int_of_char str.[13] with
  	      | 0 -> Info_ColorModel Gray
  	      | 2 -> Info_ColorModel RGB
  	      | 3 -> Info_ColorModel Index
  	      | 4 -> Info_ColorModel GrayA
  	      | 6 -> Info_ColorModel RGBA
  	      | _ -> raise Not_found in
  	    [colormodel; bdepth]
  	  with
  	  | Not_found -> [bdepth] in
  	{ header_width = w;
  	  header_height = h;
  	  header_infos = infos; }
      end
    end else raise Wrong_file_type
  with
  | _ -> close_in ic; raise Wrong_file_type;;

add_methods Png {
  check_header = check_header;
  load = Some load;
  save = Some save;
  load_sequence = None;
  save_sequence = None;
};;
