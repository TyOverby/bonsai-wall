open Tsdl

open Wall

let load_font name =
  let ic = open_in_bin name in
  let dim = in_channel_length ic in
  let fd = Unix.descr_of_in_channel ic in
  let buffer =
    Unix.map_file fd Bigarray.int8_unsigned Bigarray.c_layout false [|dim|]
    |> Bigarray.array1_of_genarray
  in
  let offset = List.hd (Stb_truetype.enum buffer) in
  match Stb_truetype.init buffer offset with
  | None -> assert false
  | Some font -> font

let font_sans = lazy (load_font "Roboto-Regular.ttf")

let normalize (dx, dy) =
  let d = sqrt (dx *. dx +. dy *. dy) in
  if d > 1.0 then
    (dx /. d, dy /. d)
  else
    (dx, dy)

let w = 1000
let h = 600
let f = try float_of_string Sys.argv.(1) with _ -> 1.0
let fw = int_of_float (f *. float w)
let fh = int_of_float (f *. float h)

let b2 x y w h = Gg.Box2.v (Gg.P2.v x y) (Gg.Size2.v w h)

let draw_arrow ~x ~y ~size color =
  Image.paint (Paint.color color)
    (Image.fill_path @@ fun t ->
     Path.move_to t x y;
     Path.line_to t (x-.size) (y+.size);
     Path.line_to t (x-.size) (y-.size);
     Path.close t;
    )

let draw_arrow' ~x ~y ~size color =
  Image.paint (Paint.color color)
    (Image.fill_path @@ fun t ->
     Path.move_to t x y;
     Path.line_to t (x-.size) (y-.size);
     Path.line_to t (x-.size/.2.0) y;
     Path.line_to t (x-.size) (y+.size);
     Path.close t;
    )

let text_arrow ?(size=16.0) ~x ~y () =
  Image.alpha 0.5 (draw_arrow ~x ~y ~size Color.blue)

let text_arrow' ?(size=16.0) ~x ~y () =
  Image.alpha 0.5 (draw_arrow' ~x ~y ~size Color.blue)

let render context sw sh t =
  let lw = float w in
  let lh = float h in
  let pw = lw *. f *. sw in
  let ph = lh *. f *. sh in
  Renderer.render context ~width:pw ~height:ph
    (Image.paint Paint.black @@
     Image.seq [
       (*text_arrow ~x:400.0 ~y:300.0 ~size:200.0 ();*)
       text_arrow' ~x:300.0 ~y:300.0 ~size:200.0 ();
       (*Image.transform (Transform.translate 10.0 10.0 (Transform.scale 5.0 5.0)) (
         Image.seq [
           (*text_arrow ~x:50.0 ~y:50.0 ();
           text_arrow' ~x:50.0 ~y:60.0 ();*)
           Image.stroke_path (Outline.make ~join:`MITER ~cap:`BUTT (*~cap:`ROUND*) ~width:(15.0) ()) @@ fun p ->
           Path.move_to p 300.0 (18.0 +. 100.0 *. sin t);
           Path.line_to p 0.0 0.0;
           Path.line_to p 300.0 0.0;
           Path.line_to p 300.0 (18.0 +. 100.0 *. sin t);
           Path.close p;
         ]
       )*)
     ])

open Tgles2

let ignore_r here = function 
  | Ok () -> ()
  | Error (`Msg s) -> Sdl.log "%s %s" (Base.Source_code_position.to_string here) s

let main () =
  Printexc.record_backtrace true;
  match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok () ->
    ignore (Sdl.gl_set_attribute Sdl.Gl.depth_size 24 : _ result);
    ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 8 : _ result);
    match
      Sdl.create_window ~w:fw ~h:fh "SDL OpenGL"
        Sdl.Window.(opengl + allow_highdpi)
    with
    | Error (`Msg e) -> Sdl.log "Create window error: %s" e; exit 1
    | Ok w ->
      (*Sdl.gl_set_attribute Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core;
        Sdl.gl_set_attribute Sdl.Gl.context_major_version 2;
        Sdl.gl_set_attribute Sdl.Gl.context_minor_version 1;*)
      ignore (Sdl.gl_set_swap_interval (-1));
      let ow, oh = Sdl.gl_get_drawable_size w in
      Sdl.log "window size: %d,%d\topengl drawable size: %d,%d" fw fh ow oh;
      let sw = float ow /. float fw and sh = float oh /. float fh in
      ignore (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
      match Sdl.gl_create_context w with
      | Error (`Msg e) -> Sdl.log "Create context error: %s" e; exit 1
      | Ok ctx ->
        let context = Renderer.create ~antialias:true () in
        let quit = ref false in
        let event = Sdl.Event.create () in
        while not !quit do
          while Sdl.poll_event (Some event) do
            match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
            | `Quit -> quit := true
            | _ -> ()
          done;
          Gl.viewport 0 0 ow oh;
          Gl.clear_color 0.3 0.3 0.32 1.0;
          Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
          Gl.enable Gl.blend;
          Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha;
          Gl.enable Gl.cull_face_enum;
          Gl.disable Gl.depth_test;
          render context sw sh (Int32.to_float (Sdl.get_ticks ()) /. 1000.0);
          Sdl.gl_swap_window w;
        done;
        Sdl.gl_delete_context ctx;
        Sdl.destroy_window w;
        Sdl.quit ();
        exit 0

let () = main ()
