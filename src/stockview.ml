open Wall
module I = Image
module P = Path
open! Core
open! Bonsai.Let_syntax

type candle =
  { center : float
  ; up1 : float
  ; up2 : float
  ; down1 : float
  ; down2 : float
  ; is_red : bool
  }

let data =
  Core.List.init 1000 ~f:(fun _ -> Random.float 10.0 -. 5.0)
  |> Core.List.folding_map
       ~init:
         { center = 0.0; up1 = 0.0; up2 = 0.0; down1 = 0.0; down2 = 0.0; is_red = true }
       ~f:(fun ({ center = acc; _ } as prev) x ->
       let r = acc +. (x *. 10.0) in
       let up1 = Float.max (r +. Random.float 10.0) (prev.center -. 10.0) in
       let up2 = Float.max (up1 +. Random.float 50.0) prev.down1 in
       let down1 = Float.min (r -. Random.float 10.0) (prev.center +. 10.0) in
       let down2 = Float.min (down1 -. Random.float 50.0) prev.up1 in
       let is_red = Core.Float.( > ) r prev.center in
       let t = { center = r; up1; up2; down1; down2; is_red } in
       t, t)
;;

module Math = struct
  type t =
    { max_y : float
    ; min_y : float
    ; lh : float
    ; lw : float
    }
end

let do_math ~lw ~lh ~mid_x ~radius =
  let start = Int.max 0 (mid_x - radius) in
  let data = List.drop data start |> Fn.flip List.take (radius * 2) in
  let min_y, max_y =
    let min_y =
      data
      |> List.map ~f:(fun { down2; _ } -> down2)
      |> List.min_elt ~compare:[%compare: float]
      |> Option.value ~default:0.0
    in
    let max_y =
      data
      |> List.map ~f:(fun { up2; _ } -> up2)
      |> List.max_elt ~compare:[%compare: float]
      |> Option.value ~default:0.0
    in
    min_y -. 3.0, max_y +. 3.0
  in
  { Math.max_y; min_y; lh; lw }
;;

let draw_chart ~mouse:(mouse_x, _) { Math.max_y; min_y; lh; lw } ~mid_x ~radius =
  let start = Int.max 0 (mid_x - radius) in
  let data = List.drop data start |> Fn.flip List.take (radius * 2) in
  let w =
    let len = List.length data in
    (lw /. Int.to_float len) -. 1.0
  in
  let mod_y y =
    let dy = max_y -. min_y in
    let rat = (y -. min_y) /. dy in
    lh *. rat
  in
  let hovered_candle = ref None in
  let candles =
    Core.List.concat_mapi
      data
      ~f:(fun i ({ center = _; up1; up2; down1; down2; is_red } as candle) ->
      let x = Int.to_float i *. (w +. 1.0) in
      let cx = x +. (w /. 2.0) in
      let red_color = Color.v_srgbi 232 100 100 in
      let green_color = Color.v_srgbi 15 232 84 in
      let color = Paint.color (if is_red then red_color else green_color) in
      let maybe_background =
        if Float.( > ) mouse_x x && Float.( <= ) mouse_x (x +. w +. 1.0)
        then (
          hovered_candle := Some candle;
          [ I.alpha
              0.15
              (I.paint color (I.fill_path (fun ctx -> Path.rect ctx ~x ~y:0.0 ~w ~h:lh)))
          ])
        else []
      in
      let maybe_filled =
        if is_red
        then
          [ I.paint
              color
              (I.fill_path (fun ctx ->
                 Path.rect ctx ~x ~y:(mod_y down1) ~w ~h:(mod_y up1 -. mod_y down1)))
          ]
        else []
      in
      maybe_background
      @ maybe_filled
      @ [ I.paint
            color
            (I.stroke_path (Outline.make ()) (fun ctx ->
               Path.rect ~x ~y:(mod_y down1) ~w ~h:(mod_y up1 -. mod_y down1) ctx;
               (* up *)
               Path.move_to ctx ~x:(x -. 1.0) ~y:(mod_y up2);
               Path.line_to ctx ~x:(x +. 1.0 +. w) ~y:(mod_y up2);
               Path.move_to ctx ~x:cx ~y:(mod_y up2);
               Path.line_to ctx ~x:cx ~y:(mod_y up1);
               (* down *)
               Path.move_to ctx ~x:(x -. 1.0) ~y:(mod_y down2);
               Path.line_to ctx ~x:(x +. 1.0 +. w) ~y:(mod_y down2);
               Path.move_to ctx ~x:cx ~y:(mod_y down2);
               Path.line_to ctx ~x:cx ~y:(mod_y down1)))
        ])
  in
  let hovered =
    match !hovered_candle with
    | Some { up1; down1; up2; down2; _ } ->
      let outer_path ctx =
        Path.rect
          ctx
          ~x:(-5.0)
          ~y:(mod_y down2)
          ~w:(lw +. 10.0)
          ~h:(mod_y up2 -. mod_y down2)
      in
      let inner_path ctx =
        Path.rect ctx ~x:0.0 ~y:(mod_y down1) ~w:lw ~h:(mod_y up1 -. mod_y down1)
      in
      I.seq
        [ I.paint (Paint.color (Color.v_srgbi 0 52 105)) (I.fill_path outer_path)
        ; I.alpha
            0.2
            (I.paint
               (Paint.color (Color.v_srgbi 0 125 255))
               (I.stroke_path Outline.default outer_path))
        ; I.paint (Paint.color (Color.v_srgbi 0 60 120)) (I.fill_path inner_path)
        ; I.alpha
            0.2
            (I.paint
               (Paint.color (Color.v_srgbi 0 125 255))
               (I.stroke_path Outline.default inner_path))
        ]
    | None -> I.empty
  in
  I.seq (hovered :: candles)
;;

let draw_demo _mx _my ~mouse ~lw ~lh ~mid_x ~radius =
  let node = ref I.empty in
  let push n = node := I.stack !node n in
  let math = do_math ~lw ~lh ~mid_x ~radius in
  push @@ draw_chart math ~mouse ~mid_x ~radius;
  !node
;;

let counter = Performance_counter.make ()

let dump_perf =
  let t0 = ref 0 in
  fun t ->
    let t = int_of_float t in
    if t <> !t0
    then (
      t0 := t;
      prerr_endline (Performance_counter.report counter);
      Performance_counter.reset counter)
;;

let init ~context:_ = ()

module Panzoom = struct
  module Model = struct
    type t =
      { dx : float
      ; mid_x : int
      ; radius : int
      ; dy : float
      ; scale : float
      ; mouse_buttons_down : Int.Set.t
      }
    [@@deriving sexp, equal]
  end

  let component =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module Evt)
      ~default_model:
        { Model.dx = 0.0
        ; dy = 0.0
        ; scale = 1.0
        ; mouse_buttons_down = Int.Set.empty
        ; mid_x = 0
        ; radius = 20
        }
      ~apply_action:
        (fun ~inject:_ ~schedule_event:_ model -> function
          | Mouse_button_down which ->
            { model with mouse_buttons_down = Int.Set.add model.mouse_buttons_down which }
          | Mouse_button_up which ->
            { model with
              mouse_buttons_down = Int.Set.remove model.mouse_buttons_down which
            }
          | Mouse_motion { x_rel; y_rel; _ } ->
            if Set.is_empty model.mouse_buttons_down
            then model
            else (
              let x_rel, y_rel = Int.to_float x_rel, Int.to_float y_rel in
              { model with
                dx = model.dx +. (x_rel *. (1.0 /. model.scale))
              ; dy = model.dy +. (y_rel *. (1.0 /. model.scale))
              })
          | Mouse_wheel { y; x; _ } ->
            let pan_by = Int.max 0 (model.radius / 20) in
            let zoom_by = Int.max 1 (model.radius / 20) in
            (* *)
            let radius = Int.max 10 (model.radius + (y * zoom_by)) in
            let mid_x = model.mid_x + (x * pan_by) in
            { model with radius; mid_x })
  ;;
end

let component ~mouse ~size () =
  let%sub panzoom = Panzoom.component in
  let%arr ((x, y) as mouse) = mouse
  and lw, lh = size
  and panzoom, inject_evt = panzoom in
  let inject e =
    let _other = Bonsai.Effect.Ignore in
    let other = Bonsai.Effect.print_s [%message (e : Evt.t)] in
    Bonsai.Effect.Many [ inject_evt e; other ]
  in
  let img =
    draw_demo x y ~mouse ~lw ~lh ~radius:panzoom.radius ~mid_x:panzoom.mid_x
    (* 
    |> I.transform (Transform.translation ~x:panzoom.dx ~y:panzoom.dy)
    |> I.transform (Transform.scale ~sx:panzoom.scale ~sy:panzoom.scale)
    *)
  in
  img, inject
;;
