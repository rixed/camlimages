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

(* $Id: conv.ml,v 1.4 2004/09/27 08:10:19 weis Exp $ *)

let jis_unicode_table = Hashtbl.create 107;;

let ic = open_in "jisuni.table" in
let cntr = ref 0 in
try while true do
  let s = input_line ic in
  let j = int_of_string ("0x" ^ String.sub s 0 4)
  and u = int_of_string ("0x" ^ String.sub s 5 4) in
  Hashtbl.add jis_unicode_table j u;
  incr cntr;
  done
with
| _ ->
  close_in ic;
  prerr_endline (Printf.sprintf "%d chars read" !cntr);;

let cntr = ref 0 in
let oc = open_out_bin "data" in
for x = 0x21 to 0x7c do
  for y = 0x21 to 0x7e do
    let x =
      try
        Hashtbl.find jis_unicode_table (x * 256 + y)
      with
      | Not_found -> 0 in
    incr cntr;
    output_char oc (char_of_int (x / 256));
    output_char oc (char_of_int (x mod 256));
  done
done;
close_out oc;
prerr_endline (Printf.sprintf "%d" !cntr);;

