open Tsdl
open Wall
open Tgles2
open Core
module I = Image
module P = Path

let or_raise here t =
  match t with
  | Ok x -> x
  | Error (`Msg e) -> raise_s [%message (here : Source_code_position.t) (e : string)]
;;

let or_log t =
  match t with
  | Ok () -> ()
  | Error (`Msg e) -> Sdl.log "%s" e
;;

let low
  ?(clear_color = 0.0, 0.0, 0.0)
  ~size:(fw, fh)
  ~init
  ~handle_event
  ~after_display
  ~f
  ()
  =
  Printexc.record_backtrace true;
  or_raise [%here] (Sdl.init Sdl.Init.video);
  or_raise [%here] (Sdl.gl_set_attribute Sdl.Gl.depth_size 24);
  or_raise [%here] (Sdl.gl_set_attribute Sdl.Gl.stencil_size 8);
  let window =
    or_raise
      [%here]
      (Sdl.create_window ~w:fw ~h:fh "Bonsai - Wall" Sdl.Window.(opengl + allow_highdpi))
  in
  or_log (Sdl.gl_set_swap_interval (-1));
  or_raise [%here] (Sdl.gl_set_attribute Sdl.Gl.stencil_size 1);
  let ctx = or_raise [%here] (Sdl.gl_create_context window) in
  let context = Renderer.create ~antialias:true () in
  let loaded = init ~context in
  let quit = ref false in
  let event = Sdl.Event.create () in
  while not !quit do
    while Sdl.poll_event (Some event) do
      let evt_opt : Evt.t option =
        match Sdl.Event.(enum (get event typ)) with
        | `Quit ->
          quit := true;
          None
        | `Mouse_button_down ->
          let idx = Sdl.Event.(get event mouse_button_button) in
          Some (Mouse_button_down idx)
        | `Mouse_button_up ->
          let idx = Sdl.Event.(get event mouse_button_button) in
          Some (Mouse_button_up idx)
        | `Mouse_motion ->
          let x = Sdl.Event.(get event mouse_motion_x) in
          let y = Sdl.Event.(get event mouse_motion_y) in
          let x_rel = Sdl.Event.(get event mouse_motion_xrel) in
          let y_rel = Sdl.Event.(get event mouse_motion_yrel) in
          Some (Mouse_motion { x; y; x_rel; y_rel })
        | `Mouse_wheel ->
          let which = Sdl.Event.(get event mouse_wheel_which) in
          let x = Sdl.Event.(get event mouse_wheel_x) in
          let y = Sdl.Event.(get event mouse_wheel_y) in
          Some (Mouse_wheel { which = Int32.to_int_exn which; x; y })
        | other ->
          print_s [%message (other : Evt.enum)];
          None
      in
      match evt_opt with
      | None -> ()
      | Some evt -> handle_event loaded evt
    done;
    let ow, oh = Sdl.gl_get_drawable_size window in
    let sw = float ow /. float fw
    and sh = float oh /. float fh in
    Gl.viewport 0 0 ow oh;
    let () =
      let r, g, b = clear_color in
      Gl.clear_color r g b 1.0
    in
    Gl.(clear (color_buffer_bit lor depth_buffer_bit lor stencil_buffer_bit));
    Gl.enable Gl.blend;
    Gl.blend_func_separate Gl.one Gl.src_alpha Gl.one Gl.one_minus_src_alpha;
    Gl.enable Gl.cull_face_enum;
    Gl.disable Gl.depth_test;
    f ~context ~sw ~sh loaded;
    Sdl.gl_swap_window window;
    after_display loaded
  done;
  Sdl.gl_delete_context ctx;
  Sdl.destroy_window window;
  Sdl.quit ()
;;

let mid
  ?(scale = 1.0)
  ?clear_color
  ~size:((w, h) as size)
  ~handle_event
  ~init
  ~after_display
  ~f
  ()
  =
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
  low ?clear_color ~size ~init ~handle_event ~after_display ~f:render ()
;;

let high ?clear_color ?scale ~size:((w, h) as size) ~init f =
  mid
    ?clear_color
    ?scale
    ~size
    ~handle_event:(fun (_, _, driver) event ->
      let _view, inject = Driver.result driver in
      Driver.schedule_event driver (inject event))
    ~init:(fun ~context ->
      let userdata = init ~context in
      let mouse_var = Bonsai.Var.create (0.0, 0.0) in
      let size_var = Bonsai.Var.create (Float.of_int w, Float.of_int h) in
      let mouse = Bonsai.Var.value mouse_var in
      let size = Bonsai.Var.value size_var in
      let driver = Driver.create (f ~mouse ~size userdata) in
      mouse_var, size_var, driver)
    ~after_display:(fun (_, _, driver) -> Driver.trigger_lifecycles driver)
    ~f:(fun ~mouse ~size (mouse_var, size_var, driver) ->
      Ui_incr.Clock.advance_clock (Driver.clock driver) ~to_:(Core.Time_ns.now ());
      Bonsai.Var.set mouse_var mouse;
      Bonsai.Var.set size_var size;
      Driver.flush driver;
      let view, _inject = Driver.result driver in
      view)
    ()
;;
