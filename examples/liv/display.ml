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

(* $Id: display.ml,v 1.32 2004/09/24 10:55:05 weis Exp $ *)

open Livmisc;;
open Color;;
open Gui;;
open Tout;;

open Gdk;;
open GDraw;;
open GMain;;

open Ximage2;;
open OXimage2;;

type filter =
    [ `TRANSPARENT_BORDER
    | `SIZE of int * int * [ `ATMOST | `ATLEAST | `NOASPECT ]
    | `NORMALIZE | `ENHANCE
    ];;

let working = ref None;;
let waiting = ref None;;
let check_waiting () = if !waiting <> None then raise Exit;;

let current_filters = (ref [] : filter list ref);;

let add_size w h =
  List.fold_right
    (fun x st ->
       match x with
       | `SIZE _ -> st
       | _ -> x :: st)
    !current_filters [`SIZE (w,h,`NOASPECT)];;

let forward_redisplay = ref (fun _ -> ());;

class type display = object
  method display : OXimage2.ximage -> unit
end;;

class win = object
  val mutable prev = (-1,-1)

  method display (ximage : OXimage2.ximage) =
    let pixmap =
      let win = window#misc#window in
      let visual = Gdk.Window.get_visual win in
      let pix =
        Gdk.Pixmap.create ~window: win
          ~depth: (Gdk.Visual.depth visual)
          ~width: ximage#width
          ~height: ximage#height () in
      let pixmap = new drawable pix in
      pixmap#put_image
        ~x:0 ~y:0
        ~width: ximage#width ~height: ximage#height
        ~xsrc:0 ~ysrc:0
        ximage#data;
      pix in
    prev <- (ximage#width, ximage#height);

    (* Resizing window *)
    (* Changes from gtk1.2 to 2.0:
       It is no longer permissable to draw directly on an arbitrary
       widget, or to set an arbitrary widget's background pixmap. If
       you need to do that, use a GtkDrawingArea or (for a toplevel) a
       GtkWindow where gtk_widget_set_app_paintable() has been called. *)
    Gdk.Window.set_back_pixmap drawing#misc#window (`PIXMAP pixmap);
    Gdk.Window.clear drawing#misc#window;

    (* We should resize the toplevel *)
    window#resize ~width: ximage#width ~height: ximage#height;
    drawing#set_size ~width: ximage#width ~height: ximage#height;
    sync ();

    set_timeout ()

  initializer
    window#event#connect#configure ~callback:
      (fun ev ->
         let w = GdkEvent.Configure.width ev
         and h = GdkEvent.Configure.height ev in
         let rec f w h =
           if (w, h) <> prev then
             (* prerr_endline (Printf.sprintf "%dx%d" w h); *)
             !forward_redisplay (add_size w h);
           let w', h' = Drawable.get_size window#misc#window in
           if (w', h') <> (w, h) then f w' h' in
         f w h;
         false);
    ()
end;;

let root_pixmap =
  lazy (
    let pix =
      GDraw.pixmap ~window
        ~width: screen_width
        ~height: screen_height () in
    pix#set_foreground `BLACK;
    pix#rectangle
      ~x:0 ~y:0 ~width: screen_width ~height: screen_height ~filled: true ();
    pix#pixmap);;

let root_drawing = lazy (new drawable !!root_pixmap);;

type root_geom = {
  width : int;
  height : int;
  xdest : int;
  xsrc : int;
  ydest : int;
  ysrc : int;
  put_width : int;
  put_height : int;
};;

let root_geom ximage x y =
  let width0 = ximage#width in
  let height0 = ximage#height in
  let xdest0 = if x < 0 then 0 else x in
  let xsrc0 = if x < 0 then -x else 0 in
  let put_width0 =
    if x + width0 > screen_width then screen_width - xdest0
    else x + width0 - xdest0 in
  let ydest0 = if y < 0 then 0 else y in
  let ysrc0 = if y < 0 then -y else 0 in
  let put_height0 =
    if y + height0 > screen_height then screen_height - ydest0
    else y + height0 - ydest0 in
  { width= width0;
    height= height0;
    xdest= xdest0;
    ydest= ydest0;
    xsrc= xsrc0;
    ysrc= ysrc0;
    put_width= put_width0;
    put_height= put_height0;
  };;

class root = object
  method display_at (ximage : OXimage2.ximage) x y =
    let geom = root_geom ximage x y in
    !!root_drawing#put_image ximage#data
      ~xsrc: geom.xsrc ~ysrc: geom.ysrc
      ~x: geom.xdest ~y: geom.ydest
      ~width: geom.put_width ~height: geom.put_height;
    Window.set_back_pixmap root_win (`PIXMAP(!!root_pixmap));
    Window.clear root_win;
    set_timeout ()
  end;;

(* src will be modified *)
let transparent_border src geom dst =
  let width = src#width
  and height = src#height in
  let src = src#data in
  let dst = dst#data in

  let color_at image x y = quick_color_parser (Image.get_pixel image ~x ~y) in

  let border = min width height / 10 + 1 in

  let doit b x y ox oy =
    let cd = color_at src x y in
    let co = color_at dst ox oy in
    let red,green,blue = color_merge co cd (border + 1) b in
    let pixel = quick_color_create ~red ~green ~blue in
    Image.put_pixel src ~x: x ~y: y ~pixel in
  
  for b = 0 to border do
    check_waiting ();
    let y = b in
    let oy = y - geom.ysrc in
    if not (oy < 0 || oy >= geom.put_height) then
      for x = b to width - b - 1 do
        let ox = x - geom.xsrc in
        if ox < 0 || ox >= geom.put_width then ()
        else doit b x y ox oy
      done;
    let y = height - b - 1 in
    let oy = y - geom.ysrc in
    if not (oy < 0 || oy >= geom.put_height) then
      for x = b to width - b - 1 do
        let ox = x - geom.xsrc in
        if ox < 0 ||ox >= geom.put_width then ()
        else doit b x y ox oy
      done;
    let x = b in
    let ox = x - geom.xsrc in
    if not (ox < 0 || ox >= geom.put_height) then
      for y = b + 1 to height - b - 2 do
        let oy = y - geom.ysrc in
        if oy < 0 || oy >= geom.put_height then ()
        else doit b x y ox oy
      done;
    let x = width - b - 1 in
    let ox = x - geom.xsrc in
    if not (ox < 0 || ox >= geom.put_width) then
      for y = b + 1 to height - b - 2 do
        let oy = y - geom.ysrc in
        if oy < 0 ||oy >= geom.put_height then ()
        else doit b x y ox oy
      done;
  done;;

class root_filter =
  object
    inherit root as super

    method display_at ximage x y =
      let geom = root_geom ximage x y in
      let dst =
        lazy (
          OXimage2.get_image
            !!root_pixmap ~x: geom.xdest ~y: geom.ydest
            ~width: geom.put_width ~height: geom.put_height) in
      List.iter
        (function
         | `TRANSPARENT_BORDER ->
            transparent_border ximage geom !!dst
         | _ -> ())
        !current_filters;
      super#display_at ximage x y
  end;;

class root_myst = object
  inherit root_filter as super

  val mutable id = None
  val mutable finish = fun () -> ()

  method display_at ximage x y =

    let geom = root_geom ximage x y in
    let tilesize = 256 in
    let tilepixels = tilesize * tilesize in
    let array =
      Array.init tilepixels
        (fun x -> x mod tilesize, x / tilesize) in
    for i = 0 to tilepixels - 1 do
      let pos = Random.int (tilepixels - 1) in
      let tmp = array.(i) in
      array.(i) <- array.(pos);
      array.(pos) <- tmp
    done;

    finish <- (fun () -> super#display_at ximage x y);

    id <- Some (Timeout.add ~ms:100 ~callback: (fun () ->
      try
        let cntr = ref 0 in
        for i = 0 to tilepixels / 10 do
          check_waiting ();
          let x, y = array.(!cntr) in
          for xx = 0 to geom.put_width / tilesize do
            for yy = 0 to geom.put_height / tilesize do
              let x = xx * tilesize + x
              and y = yy * tilesize + y in
              if x < geom.put_width && y < geom.put_height then
                !!root_drawing#put_image ximage#data
                  ~xsrc: (geom.xsrc+x) ~ysrc: (geom.ysrc+y)
                  ~x: (geom.xdest+x) ~y: (geom.ydest+y)
                  ~width: 1 ~height: 1
            done
          done;
          incr cntr;
          if !cntr = tilepixels then raise Exit
        done;
        Window.set_back_pixmap root_win (`PIXMAP (!!root_pixmap));
        Window.clear root_win;
        true
      with
      | Exit ->
        Window.set_back_pixmap root_win (`PIXMAP (!!root_pixmap));
        Window.clear root_win;
        id <- None;
        set_timeout ();
        false))

  method force_finish =
    match id with
    | Some i ->
      Timeout.remove i;
      id <- None;
      finish ();
      finish <- fun () -> ()
    | None -> ()
end;;

class root_transparent = object
  inherit root_myst as super

  method display_at ximage x y =
    let geom = root_geom ximage x y in
    let array =
      Array.init (geom.put_width * geom.put_height)
        (fun x -> x mod geom.put_width, x / geom.put_width) in
    for i = 0 to  geom.put_width * geom.put_height - 1 do
      let pos = Random.int (geom.put_width * geom.put_height - 1) in
      let tmp = array.(i) in
      array.(i) <- array.(pos);
      array.(pos) <- tmp
    done;

    let cntr = ref 0 in
    finish <- (fun () -> super#display_at ximage x y);

    id <- Some (Timeout.add ~ms:100 ~callback: (fun () ->
      try
        for i = 0 to geom.put_width * geom.put_height / 10 do
          check_waiting ();
          let x, y = array.(!cntr) in
          !!root_drawing#put_image ximage#data
            ~xsrc: (geom.xsrc+x) ~ysrc: (geom.ysrc+y)
            ~x: (geom.xdest+x) ~y: (geom.ydest+y)
            ~width: 1 ~height: 1;
          incr cntr;
          if !cntr = geom.put_width * geom.put_height then raise Exit
        done;
        Window.set_back_pixmap root_win (`PIXMAP (!!root_pixmap));
        Window.clear root_win;
        true
      with
      | Exit ->
        Window.set_back_pixmap root_win (`PIXMAP (!!root_pixmap));
        Window.clear root_win;
        id <- None;
        set_timeout ();
        false))
end;;

(*
class display_root_transparent ximage x y =
  object (self)
  inherit display_root_myst ximage x y as super

  method display =
    let orgimg = self#orgimg in
    let tmpimg = self#tmpimg in
    self#init;

    let max = 4 in
    cntr <- 1;
    id <- Some (Timeout.add ~ms:100 ~callback: (fun () ->
      for y = 0 to geom.put_height - 1 do
        for x = 0 to geom.put_width - 1 do
          let cd =
            quick_color_parser
              (Image.get_pixel
                 ximage#data ~x:(geom.xsrc+x) ~y:(geom.ysrc+y)) in
          let co =
            quick_color_parser (Image.get_pixel orgimg ~x ~y) in
          let red, green, blue = color_merge co cd max cntr in
          let pixel =
            quick_color_create ~red ~green ~blue in
          Image.put_pixel tmpimg ~x ~y ~pixel;
        done
      done;
      if cntr = max then begin
        drawing_root_pixmap#put_image tmpimg
          ~xsrc:0 ~ysrc:0 ~xdest: geom.xdest ~ydest: geom.ydest
          ~width: geom.put_width ~height: geom.put_height;
        Window.set_back_pixmap root_win ~pixmap: (`PIXMAP(pix));
        Window.clear root_win;
        finished <- true;
        self#free_tmps;
        set_timeout ();
        false
      end else begin
        drawing_root#put_image tmpimg
          ~xsrc:0 ~ysrc:0 ~xdest: geom.xdest ~ydest: geom.ydest
          ~width: geom.put_width ~height: geom.put_height;
        cntr <- cntr + 1;
        true
      end))
end;;
*)

type root_mode = [ `NONE | `CENTER | `RANDOM ];;
type transition = [ `NONE | `MYST | `TRANSPARENT ];;

let root_mode = ref (`NONE : root_mode);;
let transition = ref (`NONE : transition);;

let win = new win;;
let root = new root_filter;;
let root_myst = new root_myst;;
let root_transparent = new root_transparent;;

let root_prev_pos = ref None;;

let display_ximage ximage =
  match !root_mode with
  | `CENTER | `RANDOM ->
      let x, y =
        let w = screen_width - ximage#width
        and h = screen_height - ximage#height in
        match !root_mode with
        | `RANDOM ->
          let w = screen_width - ximage#width
          and h = screen_height - ximage#height in

          let overwrap x y =
            match !root_prev_pos with
            | None -> 0
            | Some (pw, ph, px, py) ->
              let w = min (x + w - 1) (px + pw - 1) - max x px in
              let h = min (y + h - 1) (py + ph - 1) - max y py in
              if w < 0 || h < 0 then 0 else w * h in

          let random_x_y () =
            let x = if w <= 0 then w / 2 else Random.int w
            and y = if h <= 0 then h / 2 else Random.int h in
            (x, y), overwrap x y in

          let min = ref (random_x_y ()) in
          for i = 0 to 5 do
            let (x, y), over = random_x_y () in
              if snd !min > over then min := (x, y), over
          done;
          let x, y = fst !min in
          root_prev_pos := Some (w, h, x, y);
          x, y
        | _ -> w/2, h/2 in
      begin
        match !transition with
        | `MYST -> root_myst#display_at ximage x y
        | `TRANSPARENT -> root_transparent#display_at ximage x y
        | _ -> root#display_at ximage x y
      end
  | _ -> win#display ximage;;

let resize w h cond old =
  let xmag,ymag =
    let xmag = float w /. float old#width
    and ymag = float h /. float old#height in

    let xmag,ymag =
      match cond with
      | `ATMOST ->
        let mag = if xmag > ymag then ymag else xmag in
        if mag > 1.0 then 1.0, 1.0 else mag, mag
      | `ATLEAST ->
        let mag = if xmag > ymag then xmag else ymag in
        if mag < 1.0 then 1.0, 1.0 else mag, mag
      | `NOASPECT -> xmag, ymag in

    let nw = truncate (float old#width *. xmag)
    and nh = truncate (float old#height *. ymag) in

    if nw > fst root_size || nh > snd root_size then
      let xmag = float (fst root_size) /. float old#width
      and ymag = float (snd root_size) /. float old#height in
      match cond with
      | `NOASPECT -> xmag, ymag
      | _ -> if xmag > ymag then ymag, ymag else xmag, xmag
    else xmag, ymag in

  if xmag = 1.0 && ymag = 1.0 then old else

  let nw = truncate (float old#width *. xmag)
  and nh = truncate (float old#height *. ymag) in
  prog#misc#map ();

  let fmts =
    if xmag > 1.0 && ymag > 1.0 then
      Printf.sprintf "enlarging to %dx%d" nw nh else
    if xmag < 1.0 && ymag < 1.0 then
      Printf.sprintf "reducing to %dx%d" nw nh
    else
      Printf.sprintf "resizing to %dx%d" nw nh in

  prog#set_format_string fmts;
  old#resize
    (Some (fun f -> prog#set_fraction f; check_waiting ()))
    nw nh;;

let get_hist img =
  prog#misc#map ();
  prog#set_format_string "histgram";

  let hist = Colorhist.create () in

  let width = img#width in
  let height = img#height in
  let f_height = float height in

  for y = 0 to height - 1 do
    check_waiting ();
    for x = 0 to width - 1 do
      Colorhist.store_sample hist (img#unsafe_get x y)
    done;
    prog#set_fraction (float (y + 1) /. f_height)
  done;
  hist;;

let normalize img =
  let hist = get_hist img in
  let normalizer = Colorhist.normalize 0.95 hist in

  prog#misc#map ();
  prog#set_format_string "normalizing...";

  let width = img#width
  and height = img#height in
  let f_height = float height in
  let img' = new OImages.rgb24 width height in

  for y = 0 to height - 1 do
    check_waiting ();
    for x = 0 to width - 1 do
      let rgb = img#unsafe_get x y in
      let new_rgb = normalizer rgb in
      img'#unsafe_set x y new_rgb;
    done;
    prog#set_fraction (float (y+1) /. f_height)
  done;
  img';;

let enhance img =
  let hist = get_hist img in
  let log, enhancer = Enhance.enhance 0.90 hist in

  prog#misc#map ();
  prog#set_format_string "enhancing...";

  let width = img#width
  and height = img#height in
  let f_height = float height in
  let img' = new OImages.rgb24 width height in

  for y = 0 to height - 1 do
    check_waiting ();
    for x = 0 to width - 1 do
      let rgb = img#unsafe_get x y in
      let new_rgb = enhancer rgb in
      img'#unsafe_set x y new_rgb;
    done;
    prog#set_fraction (float (y+1) /. f_height)
  done;
  img';;

let sort_filters (filters : filter list) =
  let rec get_size = function
    | [] -> []
    | `SIZE x :: _ -> [`SIZE x]
    | _ :: xs -> get_size xs in
  let rec get_normalize = function
    | [] -> []
    | `NORMALIZE :: _ -> [`NORMALIZE]
    | _ :: xs -> get_normalize xs in
  let rec get_enhance = function
    | [] -> []
    | `ENHANCE :: _ -> [`ENHANCE]
    | _ :: xs -> get_enhance xs in
  List.flatten [get_enhance filters; get_normalize filters; get_size filters];;

let cache = Cache.create 5;;
let xcache = (Cache.create 5 : (int * filter list, _) Cache.t);;

let ximage_of_image (id : int) image filters =
  let filters = sort_filters filters in
  let ximage =
    try
      let ximage = Cache.find xcache (id, filters) in
      Cache.add cache (id, filters) image;
      ximage
    with
    | Not_found ->
      let rec get_image filters =
        try Cache.find cache (id, filters) with
        | Not_found ->
          match filters with
          | [] -> image
          | `SIZE (w, h, cond) :: fs -> resize w h cond (get_image fs)
          | `NORMALIZE :: fs -> normalize (get_image fs)
          | `ENHANCE :: fs -> enhance (get_image fs) in
      let image = get_image filters in
      Cache.add cache (id, filters) image;
      prog#misc#map ();
      prog#set_format_string "mapping";
      OXimage2.of_image visual (Some prog#set_fraction) image#coerce in

  Cache.add xcache (id, filters) ximage;
  ximage;;

let current = ref None;;

let rec display id image filters =
  let start_waiting () =
    match !waiting with
    | Some (id, image, filters) ->
      prerr_endline "aborted!";
      waiting := None;
      display id image filters
    | None -> () in

  if !working <> None then begin
    (* we store it at waiting *)
    (* hoping working process may find it and move to it *)
    prerr_endline "try to abort";
    waiting := Some (id, image, filters);
  end else begin
    try
      working := Some (id, image, filters);
      let ximage = ximage_of_image id image filters in
      current := Some (id, image);
      current_filters := filters;
      display_ximage ximage;
      working := None;
      start_waiting ()
    with
    | Exit -> (* abort! *)
        working := None;
        start_waiting ()
  end;;

let redisplay new_filters =
  match !current with
  | Some (id, image) -> display id image new_filters; prog#misc#unmap ()
  | None -> ();;

forward_redisplay := redisplay;;
