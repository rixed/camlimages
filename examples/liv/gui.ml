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

(* $Id: gui.ml,v 1.25 2004/09/21 18:15:44 weis Exp $ *)

open Gdk;;
open GDraw;;
open GMain;;

GMain.Main.init ();;

let active = ref true;;

let sync () = while Glib.Main.iteration false do () done;;

let window = GWindow.window ~title: "liv" ();;
(* We should not set allow_shrink and allow_grow here. *)

window#connect#destroy ~callback:Main.quit;;
window#misc#set_size_request ~width: 1 ~height: 1 ();;
window#resize ~width: 150 ~height: 150;;

let fixed = GPack.fixed ~packing: window#add ();;
fixed#misc#set_app_paintable true;;

(* in gtk2.0 we need drawing area (drawing on fixed directory does no longer
   work) *)

let drawing = GMisc.drawing_area ~width:150 ~height:150
    ~packing: (fixed#put ~x:0 ~y:0) ~show:true ();;

(*
window#event#connect#configure (fun ev ->
  prerr_endline (Printf.sprintf "Configure %dx%d+%d+%d"
                   (GdkEvent.Configure.width ev)
                   (GdkEvent.Configure.height ev)
                   (GdkEvent.Configure.x ev)
                   (GdkEvent.Configure.y ev));
  false (* continue configure event handling *))
*)

class new_progress_bar obj = object
  inherit GRange.progress_bar obj as super
  val mutable previous = 0.0
  method set_fraction x =
    let x = floor (x *. 10.0) /. 10.0 in
    if x <> previous then begin super#set_fraction x; sync () end
end;;

let new_progress_bar =
  GtkRange.ProgressBar.make_params []
    ~cont:(fun pl ?packing ?show () ->
             GObj.pack_return
               (new new_progress_bar (GtkRange.ProgressBar.create pl))
               ~packing ~show);;

let prog1 = new_progress_bar ~packing: (fixed#put ~x:0 ~y:0) ()
;;

let visual = window#misc#visual;;
let screen_width = Screen.width ();;
let screen_height = Screen.height ();;
let colormap = Gdk.Color.get_system_colormap ();;

let quick_color_create = Truecolor.color_creator visual;;
let quick_color_parser = Truecolor.color_parser visual;;

let root_win = Window.root_parent ();;

let root_size = Drawable.get_size root_win;;

let drawing_root = new drawable root_win;;

let infowindow = GWindow.window ~title:"liv info" ~width:300 ~height:150 ();;

infowindow#misc#set_size_request ~width: 300 ~height: 150;;
infowindow#resize ~width: 300 ~height: 150;;
infowindow#connect#destroy ~callback:Main.quit;;

let imglbox0 = GPack.vbox ~packing:infowindow#add ();;

let imglbox = GPack.hbox ~packing:imglbox0#add ();;

let sb = GRange.scrollbar `VERTICAL
    ~packing:(imglbox#pack ~from:`END ~expand:false) ();;

let imglist =
  ((GList.clist ~shadow_type:`OUT
     ~columns: 1 ~packing: imglbox#add ~vadjustment:sb#adjustment ())
   : string GList.clist);;

imglist#misc#set_size_request ~width:300 ~height: 150;;

let prog2 = new_progress_bar ~packing: (imglbox0#pack ~expand: false) ();;

class prog = object
  method misc = prog1#misc
  method set_format_string s =
    prog1#set_text s;
    prog2#set_text s
  method set_fraction s =
    prog1#set_fraction s;
    prog2#set_fraction s
end;;

let prog = new prog;;
