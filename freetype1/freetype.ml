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

(* $Id: freetype.ml,v 1.7 2004/09/27 07:52:45 weis Exp $*)

type ('a, 'b) refbox = { cont : 'a; ref : 'b ref; };;

let float_of_intfrac dotbits i =
  let d = float (1 lsl dotbits) in
  (float i) /. d;;

let intfrac_of_float dotbits f =
  let d = float (1 lsl dotbits) in
  truncate (f *. d);;

let intfrac6_of_float = intfrac_of_float 6;;
let float_of_intfrac6 = float_of_intfrac 6;;
let intfrac16_of_float = intfrac_of_float 16;;
let float_of_intfrac16 = float_of_intfrac 16;;

module Freetype1 = struct

  type bbox = {
      xmin : float;
      ymin : float;
      xmax : float;
      ymax : float;
    };;

  type small_glyph_metrics = {
      small_bbox : bbox;
      small_bearingx : float;
      small_bearingy : float;
      small_advance :  float;
    };;

  type bearing_advance = {
      bearingx : float;
      bearingy : float;
      advance : float;
    };;

  (* big glyph metrics *)
  type glyph_metrics = {
      bbox : bbox;
      hori : bearing_advance;
      vert : bearing_advance;
    };;

  type engine = (Ftlow.engine, unit) refbox;;

  let init () =
    let engine = {cont = Ftlow.init (); ref = ref ()} in
    Gc.finalise (fun v -> Ftlow.close v.cont) engine;
    engine;;

  type face = (Ftlow.face, engine) refbox;;

  type face_properties = Ftlow.face_properties = {
      num_glyphs : int;
      num_max_points : int;
      num_max_contours : int;
      num_charmaps : int;
      num_names : int;
      num_faces : int;
      has_horizontal : bool;
      has_vertical : bool;
    };;

  let close_face face = Ftlow.close_face face.cont;;

  let open_face engine font =
    let face = {cont = Ftlow.open_face engine.cont font; ref = ref engine} in
    Gc.finalise (fun v -> Ftlow.close_face v.cont) face;
    face;;

  let get_face_properties face = Ftlow.get_face_properties face.cont;;

  type instance = (Ftlow.instance, face) refbox;;

  let open_instance face =
    let inst = {cont = Ftlow.open_instance face.cont; ref = ref face} in
    Gc.finalise (fun v -> Ftlow.close_instance inst.cont) inst;
    inst;;

  let set_instance_resolutions inst =
    Ftlow.set_instance_resolutions inst.cont;;

  let set_instance_charsizes inst charwidth charheight =
    Ftlow.set_instance_charsizes inst.cont
      (intfrac6_of_float charwidth)
      (intfrac6_of_float charheight);;

  let set_instance_pixelsizes inst pixwidth pixheight pointsize =
    Ftlow.set_instance_pixelsizes inst.cont
      pixwidth pixheight (intfrac6_of_float pointsize);;

  type glyph = (Ftlow.glyph, face) refbox;;

  let open_glyph face =
    let glyph = {cont = Ftlow.open_glyph face.cont; ref = ref face} in
    Gc.finalise (fun v -> Ftlow.close_glyph glyph.cont) glyph;
    glyph;;

  let load_glyph inst glyph = Ftlow.load_glyph inst.cont glyph.cont;;

  type outline = (Ftlow.outline, glyph) refbox

  let get_glyph_outline glyph =
    let outline = {cont = Ftlow.get_glyph_outline glyph.cont; ref = ref glyph}
    in
    Gc.finalise (fun v -> Ftlow.close_outline outline.cont) outline;
    outline;;

  let transform_outline outline xx xy yx yy =
    Ftlow.transform_outline outline.cont
      (intfrac16_of_float xx)
      (intfrac16_of_float xy)
      (intfrac16_of_float yx)
      (intfrac16_of_float yy);;

  let rotate_outline outline r =
    let c = cos r
    and s = sin r in
    Ftlow.transform_outline outline.cont
      (intfrac16_of_float c)
      (intfrac16_of_float (-. s))
      (intfrac16_of_float s)
      (intfrac16_of_float c);;

  let translate_outline outline xoff yoff =
    Ftlow.translate_outline outline.cont
      (intfrac6_of_float xoff)
      (intfrac6_of_float yoff);;

  let get_maximum_bbox face =
    let low = Ftlow.get_maximum_bbox face.cont in
    { xmin = float_of_intfrac6 low.Ftlow.xmin;
      ymin = float_of_intfrac6 low.Ftlow.ymin;
      xmax = float_of_intfrac6 low.Ftlow.xmax;
      ymax = float_of_intfrac6 low.Ftlow.ymax; };;

  let get_glyph_small_metrics glyph =
    let low = Ftlow.get_small_glyph_metrics glyph.cont in
    { small_bbox =
        { xmin = float_of_intfrac6 low.Ftlow.small_bbox.Ftlow.xmin;
          ymin = float_of_intfrac6 low.Ftlow.small_bbox.Ftlow.ymin;
          xmax = float_of_intfrac6 low.Ftlow.small_bbox.Ftlow.xmax;
          ymax = float_of_intfrac6 low.Ftlow.small_bbox.Ftlow.ymax; };
      small_bearingx = float_of_intfrac6 low.Ftlow.small_bearingx;
      small_bearingy = float_of_intfrac6 low.Ftlow.small_bearingy;
      small_advance = float_of_intfrac6 low.Ftlow.small_advance; };;

  let get_glyph_metrics glyph =
    let low = Ftlow.get_glyph_metrics glyph.cont in
    { bbox = { xmin = float_of_intfrac6 low.Ftlow.bbox.Ftlow.xmin;
               ymin = float_of_intfrac6 low.Ftlow.bbox.Ftlow.ymin;
               xmax = float_of_intfrac6 low.Ftlow.bbox.Ftlow.xmax;
               ymax = float_of_intfrac6 low.Ftlow.bbox.Ftlow.ymax; };
      hori = { bearingx = float_of_intfrac6 low.Ftlow.hori.Ftlow.bearingx;
               bearingy = float_of_intfrac6 low.Ftlow.hori.Ftlow.bearingy;
               advance = float_of_intfrac6 low.Ftlow.hori.Ftlow.advance };
      vert = { bearingx = float_of_intfrac6 low.Ftlow.vert.Ftlow.bearingx;
               bearingy = float_of_intfrac6 low.Ftlow.vert.Ftlow.bearingy;
               advance = float_of_intfrac6 low.Ftlow.vert.Ftlow.advance; } };;

  type instance_metrics = {
      x_ppem : int;
      y_ppem : int;
      x_scale : float;
      y_scale : float;
    };;

  let get_instance_metrics instance =
    let low = Ftlow.get_instance_metrics instance.cont in
    { x_ppem = low.Ftlow.x_ppem;
      y_ppem = low.Ftlow.y_ppem;
      x_scale = float_of_intfrac16 low.Ftlow.x_scale;
      y_scale = float_of_intfrac16 low.Ftlow.y_scale; };;

  type map_kind = Pix | Bit;;
  type map = (Ftlow.map * map_kind, unit) refbox;;

  let open_pixmap w h =
    let pixmap = {cont = (Ftlow.open_pixmap w h, Pix); ref = ref ()} in
    Gc.finalise (fun v -> Ftlow.close_map (fst pixmap.cont)) pixmap;
    pixmap;;

  let open_bitmap w h =
    let pixmap = {cont = (Ftlow.open_bitmap w h, Bit); ref = ref ()} in
    Gc.finalise (fun v -> Ftlow.close_map (fst pixmap.cont)) pixmap;
    pixmap;;

  let clear_map pixmap = Ftlow.clear_map (fst pixmap.cont);;

  (* let return_pixmap = Ftlow.return_pixmap;; *)

  let read_map pixmap =
    match pixmap.cont with
    | map, Pix -> Ftlow.read_pixmap map
    | map, Bit -> Ftlow.read_bitmap map;;

  let get_outline_pixmap engine outline pixmap =
    if snd pixmap.cont <> Pix then failwith "freetype1: wrong map type";
    Ftlow.get_outline_pixmap engine.cont outline.cont (fst pixmap.cont);;

  let get_outline_bitmap engine outline pixmap =
    if snd pixmap.cont <> Bit then failwith "freetype1: wrong map type";
    Ftlow.get_outline_bitmap engine.cont outline.cont (fst pixmap.cont);;

  let get_outline_bbox outline =
    let low =  Ftlow.get_outline_bbox outline.cont in
    { xmin = float_of_intfrac6 low.Ftlow.xmin;
      ymin = float_of_intfrac6 low.Ftlow.ymin;
      xmax = float_of_intfrac6 low.Ftlow.xmax;
      ymax = float_of_intfrac6 low.Ftlow.ymax; };;

  let get_glyph_pixmap glyph pixmap x y =
    if snd pixmap.cont <> Pix then failwith "freetype1: wrong map type";
    Ftlow.get_glyph_pixmap glyph.cont (fst pixmap.cont)
      (intfrac6_of_float x)
      (intfrac6_of_float y);;

  let get_glyph_bitmap glyph pixmap x y =
    if snd pixmap.cont <> Bit then failwith "freetype1: wrong map type";
    Ftlow.get_glyph_bitmap glyph.cont (fst pixmap.cont)
      (intfrac6_of_float x)
      (intfrac6_of_float y);;

  type charmap = (Ftlow.charmap, face) refbox;;

  let get_charmap_ids face =
    Ftlow.get_charmap_ids face.cont;;

  let get_charmap face id =
    let charmap = {cont = Ftlow.get_charmap face.cont id; ref = ref face} in
    Gc.finalise (fun v-> Ftlow.close_charmap v.cont) charmap;
    charmap;;

  let char_index charmap = Ftlow.char_index charmap.cont;;

  let get_num_glyphs face = Ftlow.get_num_glyphs face.cont;;

  (**************************************** find encodings (by Koji Kagawa) *)

  let find_charmap_unicode face =
    let n = Ftlow.get_charmap_count face.cont in
    let rec loop i =
      if i < n then
        let (platform, encoding) = Ftlow.get_charmap_id face.cont i in
        match (platform, encoding) with
        | 0, 0 -> i (* Apple Unicode *)
        | 3, 1 -> i (* Windows Unicode *)
        | _ -> loop (i + 1)
      else failwith "no Unicode mapping" in
    let idx = loop 0 in
    get_charmap face idx;;

  let find_charmap face =
    try find_charmap_unicode face (* try unicode first *)
    with
    | _ ->
      let n = Ftlow.get_charmap_count face.cont in
      let rec loop i =
        if i < n then
          let (platform, encoding) = Ftlow.get_charmap_id face.cont i in
          match (platform, encoding) with
          | 1, 0 -> i (* Apple Roman *)
          | 3, 0 -> i (* Windows Symbol *)
          | _ -> loop (i + 1)
        else failwith "no known mapping" in
      let idx = loop 0 in
      get_charmap face idx;;

  type outline_tag = Ftlow.outline_tag =
     | On_point | Off_point_conic | Off_point_cubic;;

  type outline_contents = {
      n_contours : int;
      n_points : int;
      points : (float * float) array;
      tags : outline_tag array;
      contours : int array;
    };;

  let get_outline_contents outline =
    let oc = Ftlow.get_outline_contents outline.cont in
    { n_contours= oc.Ftlow.n_contours;
      n_points = oc.Ftlow.n_points;
      points =
        Array.map
          (fun x, y ->
             float_of_intfrac6 x, float_of_intfrac6 y)
          oc.Ftlow.points;
      tags = oc.Ftlow.tags;
      contours = oc.Ftlow.contours; };;

end;;

type t = Freetype1.engine;;

let init () = Freetype1.init ();;

type matrix = { ft_xx : float; ft_xy : float; ft_yx : float; ft_yy : float; };;
type vector = { ft_x : float; ft_y : float };;

let matrix_rotate r =
  let c = cos r
  and s = sin r in
  {ft_xx = c; ft_xy = -.s; ft_yx = s; ft_yy = c};;

(*********************************************************** wrapped bitmaps *)

type map = {
    mutable curw : int;
    mutable curh : int;
    mutable map : Freetype1.map;
    mutable curxmin : int;
    mutable curymax : int
  };;

(************************************************************** wrapped face *)

type face = {
    face : Freetype1.face;
    the_instance : Freetype1.instance;
    the_glyph : Freetype1.glyph;
    the_map : map;
    mutable the_charmap : Freetype1.charmap;
    mutable the_matrix : matrix;
    mutable the_vector : vector;
  };;

type face_info = {
    num_faces : int;
    num_glyphs : int;
    family_name : string;
    style_name : string;
    has_horizontal : bool;
    has_vertical : bool;
    has_kerning : bool;
    is_scalable : bool;
    is_sfnt : bool;
    is_fixed_width : bool;
    has_fixed_sizes : bool;
    has_fast_glyphs : bool;
    has_glyph_names : bool;
    has_multiple_masters : bool
  };;

(************************************************************* glyph metrics *)

type bbox = Freetype1.bbox = {
    xmin : float;
    ymin : float;
    xmax : float;
    ymax : float;
  };;

type bearing_advance = Freetype1.bearing_advance = {
    bearingx : float;
    bearingy : float;
    advance : float;
  };;

type glyph_metrics = {
    gm_width : float;
    gm_height : float;
    gm_hori : bearing_advance;
    gm_vert : bearing_advance;
  };;

(************************************************** wrapped bitmap functions *)

type render_mode = Render_Normal | Render_Mono;;

let create_map init_w init_h =
  { curw = init_w;
    curh= init_h;
    curxmin= 0;
    curymax= 0;
    map= Freetype1.open_pixmap init_w init_h };;

let set_map mode map w h xmin ymax =
  map.curw <- w;
  map.curh <- h;
  begin match mode with
  | Render_Normal -> map.map <- Freetype1.open_pixmap map.curw map.curh
  | Render_Mono -> map.map <- Freetype1.open_bitmap map.curw map.curh
  end;
  map.curxmin <- xmin;
  map.curymax <- ymax;;

let mapbbox curx cury bbox =
  let plus = 1 in

  let xmin = curx +. bbox.xmin
  and ymin = cury -. bbox.ymin in
  let xmax = curx +. bbox.xmax
  and ymax = cury -. bbox.ymax in
  let xmin' = truncate xmin - plus in
  let ymax' = truncate ymax - plus in
  let ymin' = truncate (ceil ymin) + plus in
  let xmax' = truncate (ceil xmax) + plus in

  let pw = xmax' - xmin' + 1
  and ph = ymin' - ymax' + 1 in

  let xoff = curx -. float xmin'
  and yoff = float ymin' -. cury in (* the map is upside down *)

  pw, ph, xmin', ymax', xoff, yoff;;

(**************************************************** wrapped face functions *)

let new_face t font idx =
  if idx <> 0
  then prerr_endline "Warning: Freetype1 can read the first font only";
  let face1 = Freetype1.open_face t font in
  let prop = Freetype1.get_face_properties face1 in
  let instance1 = Freetype1.open_instance face1 in
  let glyph1 = Freetype1.open_glyph face1 in
  let info = {
      num_faces = prop.Freetype1.num_faces;
      num_glyphs = prop.Freetype1.num_glyphs;
      family_name = "";
      style_name = "";
      has_horizontal = prop.Freetype1.has_horizontal;
      has_vertical = prop.Freetype1.has_vertical;
      has_kerning = false;
      is_scalable = true;
      is_sfnt = true;
      is_fixed_width = false; (* we don't know it *)
      has_fixed_sizes = false;
      has_fast_glyphs = false;
      has_glyph_names = prop.Freetype1.num_names <> 0;
      has_multiple_masters = false;
    } in
  { face = face1;
    the_instance = instance1;
    the_glyph = glyph1;
    the_charmap = Freetype1.find_charmap face1;
    the_map = create_map 20 20;
    the_matrix = {ft_xx = 1.0; ft_xy = 0.0; ft_yx=0.0; ft_yy = 1.0};
    the_vector = {ft_x = 0.0; ft_y = 0.0};
  }, info;;

let get_num_glyphs face = Freetype1.get_num_glyphs face.face;;

type char_index = int;; (* abstracted *)

let int_of_char_index = fun x -> x;;
let char_index_of_int = fun x -> x;;

let set_char_size face char_w char_h res_h res_v =
  Freetype1.set_instance_resolutions face.the_instance res_h res_v;
  Freetype1.set_instance_charsizes face.the_instance char_w char_h;;

let set_pixel_sizes face pixw pixh =
  Freetype1.set_instance_pixelsizes face.the_instance pixw pixh 10.0;;

type charmap = { platform_id : int; encoding_id : int };;

let get_charmaps face =
  List.map
    (fun (_, (p, e)) ->
      { platform_id = p; encoding_id = e; })
    (Freetype1.get_charmap_ids face.face);;

let set_charmap face charmap =
  let lst =
    List.map
      (fun (i, (p, e)) ->
         { platform_id = p; encoding_id = e; }, i)
      (Freetype1.get_charmap_ids face.face) in
  try
    face.the_charmap <-
       Freetype1.get_charmap face.face (List.assoc charmap lst)
  with
  | Not_found -> failwith "no such charmap";;

let get_char_index face n = Freetype1.char_index face.the_charmap n;;

(******************************************* wrapped glyph metrics functions *)

let get_glyph_metrics face =
  let gm1 = Freetype1.get_glyph_metrics face.the_glyph in
  { gm_width = gm1.Freetype1.bbox.xmax -. gm1.Freetype1.bbox.xmin;
    gm_height = gm1.Freetype1.bbox.ymax -. gm1.Freetype1.bbox.ymin;
    gm_hori = gm1.Freetype1.hori;
    gm_vert = gm1.Freetype1.vert; };;

(******************************************** wrapped size metrics functions *)

type size_metrics = Freetype1.instance_metrics = {
    x_ppem : int;
    y_ppem : int;
    x_scale : float;
    y_scale : float;
  };;

let get_size_metrics face = Freetype1.get_instance_metrics face.the_instance;;

(********************************************** load/render glyphs functions *)

type load_flag =
   | Load_no_scale | Load_no_hinting;;
(* if you give [], freetype loads glyphs with scaling and hinting *)

let load_glyph face id flags =
  Freetype1.load_glyph face.the_instance face.the_glyph id
    (not (List.mem Load_no_scale flags))
    (not (List.mem Load_no_hinting flags));
  (* bug: horizontal direction only *)
  let gm = get_glyph_metrics face in
  let adv = gm.gm_hori.advance in
  face.the_matrix.ft_xx *. adv,
  face.the_matrix.ft_yx *. adv;;

let load_char face ch flags = load_glyph face (get_char_index face ch) flags;;

let engine_of_face face = !(face.face.ref);;

let render_glyph_of_face face mode =
  let outline = Freetype1.get_glyph_outline face.the_glyph in
  Freetype1.transform_outline
    outline face.the_matrix.ft_xx face.the_matrix.ft_xy
    face.the_matrix.ft_yx face.the_matrix.ft_yy;
  Freetype1.translate_outline
    outline face.the_vector.ft_x face.the_vector.ft_y;
  let bbox = Freetype1.get_outline_bbox outline in
  let w, h, xmin, ymax, xoff, yoff = mapbbox 0.0 0.0 bbox in
  Freetype1.translate_outline outline xoff yoff;
  set_map mode face.the_map w h xmin ymax;
  Freetype1.clear_map face.the_map.map;
  match mode with
  | Render_Normal ->
      Freetype1.get_outline_pixmap
        (engine_of_face face) outline face.the_map.map
  | Render_Mono ->
      Freetype1.get_outline_bitmap
        (engine_of_face face) outline face.the_map.map;;

let render_glyph face idx flags mode =
  let adv = load_glyph face idx flags in
  render_glyph_of_face face mode;
  adv;;

let render_char face ch flags mode =
  let adv = load_char face ch flags in
  render_glyph_of_face face mode;
  adv;;

let set_transform face mat vec =
  face.the_matrix<- mat;
  face.the_vector<- vec;;

(************************************************************ bitmap reading *)

type bitmap_info = {
    bitmap_left : int;
    bitmap_top : int;
    bitmap_width : int;
    bitmap_height : int;
  };;

let get_bitmap_info face =
  { bitmap_left = face.the_map.curxmin;
    bitmap_top = -face.the_map.curymax; (* !!! *)
    bitmap_width = face.the_map.curw;
    bitmap_height = face.the_map.curh; };;

let read_bitmap face x y =
  let lev = (Freetype1.read_map face.the_map.map x y) * 64 - 1 in
  if lev < 0 then 0 else lev;;

(************************************************************** outline info *)

type outline_tag = Freetype1.outline_tag =
   | On_point | Off_point_conic | Off_point_cubic;;

type outline_contents = Freetype1.outline_contents = {
    n_contours : int;
    n_points : int;
    points : (float * float) array;
    tags : outline_tag array;
    contours : int array;
  };;

let get_outline_contents face =
  Freetype1.get_outline_contents (Freetype1.get_glyph_outline face.the_glyph);;
