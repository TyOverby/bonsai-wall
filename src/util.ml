open Tsdl
open Wall
open Tgles2
module I = Image
module P = Path

let or_raise t =
  match t with
  | Ok x -> x
  | Error (`Msg e) -> failwith e
;;

let or_log t =
  match t with
  | Ok () -> ()
  | Error (`Msg e) -> Sdl.log "%s" e
;;

let low ~size:(fw, fh) ~init ~f =
  Printexc.record_backtrace true;
  or_raise (Sdl.init Sdl.Init.video);
  or_raise (Sdl.gl_set_attribute Sdl.Gl.depth_size 24);
  or_raise (Sdl.gl_set_attribute Sdl.Gl.stencil_size 8);
  let window =
    or_raise
      (Sdl.create_window ~w:fw ~h:fh "Bonsai - Wall" Sdl.Window.(opengl + allow_highdpi))
  in
  or_log (Sdl.gl_set_swap_interval (-1));
  or_raise (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
  let ctx = or_raise (Sdl.gl_create_context window) in
  let context = Renderer.create ~antialias:true () in
  let loaded = init ~context in
  let quit = ref false in
  let event = Sdl.Event.create () in
  while not !quit do
    while Sdl.poll_event (Some event) do
      match Sdl.Event.enum (Sdl.Event.get event Sdl.Event.typ) with
      | `Quit -> quit := true
      | _ -> ()
    done;
    let ow, oh = Sdl.gl_get_drawable_size window in
    let sw = float ow /. float fw
    and sh = float oh /. float fh in
    Gl.viewport 0 0 ow oh;
    Gl.clear_color 0.3 0.3 0.32 1.0;
    Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
    Gl.enable Gl.blend;
    Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha;
    Gl.enable Gl.cull_face_enum;
    Gl.disable Gl.depth_test;
    f ~context ~sw ~sh loaded;
    Sdl.gl_swap_window window
  done;
  Sdl.gl_delete_context ctx;
  Sdl.destroy_window window;
  Sdl.quit ()
;;

let mid ?(scale = 1.0) ~size:((w, h) as size) ~init ~f () =
  let f, draw = scale, f in
  let render ~context ~sw ~sh state =
    let lw = float w in
    let lh = float h in
    let width = lw *. f *. sw in
    let height = lh *. f *. sh in
    let mouse =
      let _, (x, y) = Sdl.get_mouse_state () in
      let x = float x /. f
      and y = float y /. f in
      x, y
    in
    let size = lw, lh in
    let demo = draw ~mouse ~size state in
    Renderer.render
      context
      ~width
      ~height
      (I.seq [ I.transform (Transform.scale ~sx:(sw *. f) ~sy:(sh *. f)) demo ])
  in
  low ~size ~init ~f:render
;;

let high ?scale ~size:((w, h) as size) ~init f =
  mid
    ?scale
    ~size
    ~init:(fun ~context ->
      let userdata = init ~context in
      let mouse_var = Bonsai.Var.create (0.0, 0.0) in
      let size_var = Bonsai.Var.create (Float.of_int w, Float.of_int h) in
      let mouse = Bonsai.Var.value mouse_var in
      let size = Bonsai.Var.value size_var in
      let driver = Driver.create (f ~mouse ~size userdata) in
      mouse_var, size_var, driver)
    ~f:(fun ~mouse ~size (mouse_var, size_var, driver) ->
      Ui_incr.Clock.advance_clock (Driver.clock driver) ~to_:(Core.Time_ns.now ());
      Bonsai.Var.set mouse_var mouse;
      Bonsai.Var.set size_var size;
      Driver.flush driver;
      let img = Driver.result driver in
      let () = Driver.trigger_lifecycles driver in
      img)
    ()
;;
