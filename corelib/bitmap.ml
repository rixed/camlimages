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

(* $Id: bitmap.ml,v 1.23 2004/09/24 14:27:24 weis Exp $*)

let debug = ref false;;
let debugs s = if !debug then prerr_endline s;;

let maximum_live = ref 0;; (* around 3M words for example *)
let maximum_block_size = ref (!maximum_live / 10);; (* default 300K words *)
(* see Temp to set temp file directory *)

type block_data =
   | InMemory of string
   | Swapped
   | Destroyed;;

type block = {
    block_width: int;
    block_height: int;
    block_size: int;
    mutable block_data: block_data;
    mutable last_used: float;
    swap: string option
  };;

let swappable_blocks = ref [];;

(* wrapped String.create *)
let string_create s =
  try String.create s with Invalid_argument _ -> raise Out_of_memory;;

module type Bitdepth = sig
  val bytes_per_pixel : int
end;;

module Make(B:Bitdepth) = struct
  open B;;

  type t = {
    (* The whole size *)
    bmap_width : int;
    bmap_height : int;
    (* block partition size *)
    block_size_width : int;
    block_size_height : int;
    (* number of block partitions *)
    blocks_x : int;
    blocks_y : int;
    data : block array array;
    access : int -> int -> (string * int);
    };;

  (****************************************************************************)
  (*                            Destruction                                   *)
  (****************************************************************************)

  let destroy t = ();; (* do nothing... *)

  let destroy_block blk =
    if blk.block_data = Destroyed then () else begin
      match blk.swap with
      | Some fname ->
        begin match blk.block_data with
        | Swapped -> Sys.remove fname; blk.block_data <- Destroyed
        | _ -> () end
      | None -> ()
    end;
    swappable_blocks :=
      List.fold_right
        (fun blk' st ->
           if blk == blk' then st else blk' :: st)
        !swappable_blocks [];;

  let fill_string buf init =
    (* fill string with init quickly (hopefully) *)
    let fulllength = String.length buf in
    let halflength = fulllength / 2 in
    let rec sub = function
      | 0 ->
        let len = String.length init in
        String.unsafe_blit init 0 buf 0 len;
        sub len
      | x when x <= halflength ->
        String.unsafe_blit buf 0 buf x x;
        sub (x * 2)
      | x (* when x > halflength *) ->
        String.unsafe_blit buf 0 buf x (fulllength - x) in
    sub 0;;

  let check_init init =
    match init with
    | Some v ->
        if String.length v <> bytes_per_pixel
        then failwith "bitmap fill value is incorrect"
    | None -> ();;

  let memory width height init =
    (* try to have it whole in memory *)
    (* first, try to make a huge string *)
    check_init init;
    try
      let buf = string_create (width * height * bytes_per_pixel) in
      begin match init with
      | Some v -> fill_string buf v
      | None -> ()
      end;
      { bmap_width = width;
        bmap_height = height;
        block_size_width = width;
        block_size_height = height;
        blocks_x = 1;
        blocks_y = 1;
        data =
           [|
              [| { block_width = width;
                   block_height = height;
                   block_data = InMemory buf;
                   block_size = width * height * bytes_per_pixel;
                   last_used = 0.0;
                   swap = None; }
              |]
           |];
        access = (fun x y -> buf, (y * width + x) * bytes_per_pixel)
      }
    with
    | Out_of_memory ->
      (* Out of memory? then let's try to use scanlined format *)
      let bufs =
        Array.init height
          (fun _ -> string_create (width * 1 * bytes_per_pixel)) in
      begin match init with
      | Some v -> Array.iter (fun s -> fill_string s v) bufs
      | None -> ()
      end;
      { bmap_width = width;
        bmap_height = height;
        block_size_width = width;
        block_size_height = 1;
        blocks_x = 1;
        blocks_y = height;
        data =
          [| Array.init height
               (fun h -> {
                  block_width = width;
                  block_height = 1;
                  block_data = InMemory bufs.(h);
                  block_size = width * 1 * bytes_per_pixel;
                  last_used = 0.0;
                  swap = None;
                  }) |];
        access = (fun x y -> bufs.(y), x * bytes_per_pixel)
      };;

  let swap_out = function
    | {block_data = Destroyed} -> failwith "swap_out: Already destroyed"
    | {swap = None} -> failwith "No swap file set"
    | {swap = Some fname; block_data = InMemory s; block_size = size} as blk ->
      begin try
        debugs (Printf.sprintf "swap out %s" fname (* blk.block_size*));
        let oc = open_out_bin fname in
        output oc s 0 size;
        close_out oc;
        blk.block_data <- Swapped
      with
      | e -> prerr_endline "Swap-out failed"; raise e
      end
    | _ -> ();;

  let touch_block blk = blk.last_used <- Sys.time ();;

  let swap_out_eldest words =
    let sorted =
      Sort.list (fun b1 b2 -> b1.last_used < b2.last_used) !swappable_blocks in
    let rec swapper sorted i =
     match sorted with
      | [] -> ()
      | x :: xs ->
        swap_out x;
        swapper xs
          (i - (x.block_size + Camlimages.word_size - 1) /
               Camlimages.word_size) in
    swapper sorted words;;

  let require bytes =
    let words = (bytes + Camlimages.word_size - 1) / Camlimages.word_size in
    let stat = Gc.stat () in
    let over = stat.Gc.live_words + words - !maximum_live in
    if over > 0 then swap_out_eldest over;;

  let swap_in = function
    | {block_data = Destroyed} -> raise (Failure "swap_in: Already destroyed")
    | {block_data = InMemory s} as blk ->
      touch_block blk;
      s
    | {swap = Some fname; block_data = Swapped; block_size = size} as blk ->
       begin try
         debugs ("swap in "^fname);
         require size;
         let ic = open_in_bin fname in
         let s = string_create size in
         really_input ic s 0 size;
         close_in ic;
         blk.block_data <- InMemory s;
         Sys.remove fname;
         touch_block blk;
         s
       with
       | e -> prerr_endline
                (Printf.sprintf "Swap-in failed (%s)" (Printexc.to_string e));
              raise e
       end
    | _ -> assert false;;

  let alloc_swappable_block width height init =
    let size = bytes_per_pixel * width * height in
    require size;
    let s = string_create size in
    begin match init with
    | Some v -> fill_string s v
    | None -> ()
    end;
    let blk =
      { block_width = width;
        block_height = height;
        block_size = size;
        block_data = InMemory s;
        last_used = Sys.time ();
        swap = Some (Tmpfile.new_tmp_file_name "swap") } in
    Gc.finalise destroy_block blk;
    swappable_blocks := blk :: !swappable_blocks;
    blk;;

  (****************************************************************************)
  (*                            Creation functions                            *)
  (****************************************************************************)

  let create width height init =
    if !maximum_live <= 0 then memory width height init else begin
      check_init init;
      (* determine the block size *)
      let rec get_block_size p =
        let whole_words =
          (bytes_per_pixel * width * height + Camlimages.word_size - 1) /
          Camlimages.word_size in
        let pp = p * p in
        if whole_words / pp > !maximum_block_size
        then get_block_size (p + 1)
        else p in
      let rec alloc_test_block p =
        let block_size_width =
              width / p + (if width mod p <> 0 then 1 else 0)
        and block_size_height =
              height / p + (if height mod p <> 0 then 1 else 0) in
        try
          p, string_create
               (block_size_width * block_size_height * bytes_per_pixel)
        with
        | Out_of_memory -> alloc_test_block (p + 1) in
      let blocks, test_block = alloc_test_block (get_block_size 1) in
      (* use the block so that it is not GCed too early *)
      test_block.[0] <- '0';

      (* Create bitmap *)
      let blocks_x = blocks
      and blocks_y = blocks in

      let block_size_width =
        width / blocks_x + (if width mod blocks_x <> 0 then 1 else 0)
      and block_size_height =
        height / blocks_x + (if height mod blocks_x <> 0 then 1 else 0) in

      debugs (Printf.sprintf "creating %d x %d blocks (%dx%d)"
                blocks blocks block_size_width block_size_height);
      let data =
        Array.init blocks_x
          (fun x ->
             Array.init blocks_y
               (fun y ->
                  let w =
                    if x <> blocks_x - 1 then block_size_width
                    else width - block_size_width * (blocks_x - 1)
                  and h =
                    if y <> blocks_y - 1 then block_size_height
                    else height - block_size_height * (blocks_y - 1) in
                  alloc_swappable_block w h init)) in
      let t =
        { bmap_width = width;
          bmap_height = height;
          block_size_width = block_size_width;
          block_size_height = block_size_height;
          blocks_x = blocks_x;
          blocks_y = blocks_y;
          data = data;
          access = (
            if blocks_x = 1 && blocks_y = 1 then begin
              let the_blk = data.(0).(0) in
              fun x y ->
                let str = swap_in the_blk in
                let pos =
                  (the_blk.block_width * (y mod block_size_height) +
                   (x mod block_size_width)) * bytes_per_pixel in
                str, pos
            end else begin
              fun x y ->
                let bx = x / block_size_width
                and by = y / block_size_height in
                let blk = data.(bx).(by) in
                let str = swap_in blk in
                let pos =
                  (blk.block_width * (y mod block_size_height) +
                     (x mod block_size_width)) * bytes_per_pixel in
                str, pos
            end);
        } in
      t
    end;;

  let create_with width height buf =
    let size = width * height * bytes_per_pixel in
    { bmap_width = width;
      bmap_height = height;
      block_size_width = width;
      block_size_height = height;
      blocks_x = 1;
      blocks_y = 1;
      data =
        [| [| { block_width = width;
                block_height = height;
                block_data = InMemory buf;
                block_size = width * height * bytes_per_pixel;
                last_used = 0.0;
                swap = None;
               } |] |];
      access = (fun x y -> buf, (y * width + x) * bytes_per_pixel);
    };;

  (****************************************************************************)
  (*                            Tool functions                                *)
  (****************************************************************************)

  let access t = t.access;;

  (* strip access *)
  (* Here, "strip" is a rectangle region with height 1 *)

  let get_strip t x y w =
    (* No region checks for performance. You should wrap this to make safe
       in your applications. *)
    match t.blocks_x, t.blocks_y with
    | 1, _ -> (* optimized *)
      let bly = y / t.block_size_height in
      let y' = y mod t.block_size_height in
      let blk = t.data.(0).(bly) in
      let src = swap_in blk in
      let size = w * bytes_per_pixel in
      let adrs = (blk.block_width * y' + x) * bytes_per_pixel in
      let str = string_create size in
      String.unsafe_blit src adrs str 0 size;
      str
    | _, _ ->
      let bly = y / t.block_size_height in
      let y' = y mod t.block_size_height in
      let str = string_create (w * bytes_per_pixel) in
      let blx_start = x / t.block_size_width in
      let blx_last = (x + w - 1) / t.block_size_width in
      for blx = blx_start to blx_last do
        let blk = t.data.(blx).(bly) in
        let src = swap_in blk in
        let x1 =
          if blx = blx_start then x mod t.block_size_width else 0 in
        let x2 =
          if blx = blx_last then (x + w - 1) mod t.block_size_width else
          (blk.block_width - 1) in
        let w' = x2 - x1 + 1 in
        let size = w' * bytes_per_pixel in
        let adrs = (blk.block_width * y' + x1) * bytes_per_pixel in
        let offset =
          if blx = blx_start then 0
          else (t.block_size_width * blx - x) * bytes_per_pixel in
        String.unsafe_blit src adrs str offset size
      done;
      str;;

  let set_strip t x y w str =
    (* No region checks for performance. You should wrap this to make safe
       in your applications. *)
    match t.blocks_x, t.blocks_y with
    | 1, _ -> (* optimized *)
      let bly = y / t.block_size_height in
      let y' = y mod t.block_size_height in
      let blk = t.data.(0).(bly) in
      let dst = swap_in blk in
      let size = w * bytes_per_pixel in
      let adrs = (blk.block_width * y' + x) * bytes_per_pixel in
      String.unsafe_blit str 0 dst adrs size
    | _, _ ->
      let bly = y / t.block_size_height in
      let y' = y mod t.block_size_height in
      let blx_start = x / t.block_size_width in
      let blx_last = (x + w - 1) / t.block_size_width in
      for blx = blx_start to blx_last do
        let blk = t.data.(blx).(bly) in
        let dst = swap_in blk in
        let x1 =
          if blx = blx_start then x mod t.block_size_width else 0 in
        let x2 =
          if blx = blx_last then (x + w - 1) mod t.block_size_width else
          (blk.block_width - 1)
        in
        let w' = x2 - x1 + 1 in
        let size = w' * bytes_per_pixel in
        let adrs = (blk.block_width *  y' + x1) * bytes_per_pixel in
        let offset =
          if blx = blx_start then 0
          else (t.block_size_width * blx - x) * bytes_per_pixel
        in
        String.unsafe_blit str offset dst adrs size
      done;;

  (* scanline access (special case of strip access) *)
  let get_scanline t y = get_strip t 0 y t.bmap_width;;

  let set_scanline t y str =
    if String.length str <> t.bmap_width * B.bytes_per_pixel then
      failwith
        (Printf.sprintf "scan=%d width=%d bbp=%d"
           (String.length str) t.bmap_width B.bytes_per_pixel);
    set_strip t 0 y t.bmap_width str;;

  (* dump : of course this does not work for large images *)

  let dump t =
    let size = bytes_per_pixel * t.bmap_width * t.bmap_height in
    match t.blocks_x, t.blocks_y with
    | 1, 1 -> swap_in t.data.(0).(0)
    | 1, h ->
      let s = string_create size in
      let scanline_size = bytes_per_pixel * t.bmap_width in
      for y = 0 to h - 1 do
        let str = swap_in t.data.(0).(y) in
        String.unsafe_blit str 0 s (scanline_size * y) scanline_size
      done;
      s
    | w, h ->
      let s = string_create size in
      for x = 0 to w - 1 do
        for y = 0 to h - 1 do
          let blk = t.data.(x).(y) in
          let str = swap_in blk in
          let scanline_size = bytes_per_pixel * blk.block_width in
          for i = 0 to blk.block_height - 1 do
            String.unsafe_blit str (scanline_size * i)
              s (((y * t.block_size_height + i) * t.bmap_width +
                    x * t.block_size_width) * bytes_per_pixel)
              scanline_size
          done
        done
      done;
      s;;

  (* sub-bitmap *)
  let sub t x y w h =
    Region.check t.bmap_width t.bmap_height x y;
    Region.check t.bmap_width t.bmap_height (x + w - 1) (y + h - 1);
    let dst = create w h None in
    try
      for i = 0 to h - 1 do set_scanline dst i (get_strip t x (y + i) w) done;
      dst
    with
    | e -> destroy dst; raise e;;

  let copy t = sub t 0 0 t.bmap_width t.bmap_height;;

  let blit src sx sy dst dx dy w h =
    Region.check src.bmap_width src.bmap_height sx sy;
    Region.check src.bmap_width src.bmap_height (sx + w - 1) (sy + h - 1);
    Region.check dst.bmap_width dst.bmap_height dx dy;
    Region.check dst.bmap_width dst.bmap_height (dx + w - 1) (dy + h - 1);
    for i = 0 to h - 1 do
      set_strip dst dx (dy + i) w (get_strip src sx (sy + i) w)
    done;;

end;;
