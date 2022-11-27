open! Base
open! Bonsai
module Incr = Ui_incr

module Action = struct
  type ('dynamic_action, 'static_action) t =
    | Dynamic of 'dynamic_action
    | Static of 'static_action
end

type ('model, 'dynamic_action, 'static_action, 'result) unpacked =
  { model_var : 'model Incr.Var.t
  ; clock : Incr.Clock.t
  ; default_model : 'model
  ; inject : ('dynamic_action, 'static_action) Action.t -> unit Ui_effect.t
  ; dynamic_apply_action_incr :
      (schedule_event:(unit Ui_effect.t -> unit) -> 'model -> 'dynamic_action -> 'model)
      Incr.t
  ; dynamic_apply_action :
      (schedule_event:(unit Ui_effect.t -> unit) -> 'model -> 'dynamic_action -> 'model)
      Incr.Observer.t
  ; static_apply_action :
      schedule_event:(unit Ui_effect.t -> unit) -> 'model -> 'static_action -> 'model
  ; result : 'result Incr.Observer.t
  ; result_incr : 'result Incr.t
  ; lifecycle : Bonsai.Private.Lifecycle.Collection.t Incr.Observer.t
  ; lifecycle_incr : Bonsai.Private.Lifecycle.Collection.t Incr.t
  ; queue : ('dynamic_action, 'static_action) Action.t Queue.t
  ; mutable last_lifecycle : Bonsai.Private.Lifecycle.Collection.t
  }

type 'r t = T : (_, _, _, 'r) unpacked -> 'r t

let create (type r) ?(clock = Incr.clock) (computation : r Bonsai.Computation.t) : r t =
  let (Bonsai.Private.Computation.T
        { t = component_unpacked
        ; dynamic_action = _
        ; static_action = _
        ; apply_static
        ; model = { default = default_model; _ }
        })
    =
    computation |> Bonsai.Private.reveal_computation
  in
  let environment = Bonsai.Private.Environment.empty in
  let starting_model = default_model in
  let model_var = Incr.Var.create starting_model in
  (* Sadly the only way to give a name to the existential type that we just introduced
     into the environment is by defining a function like this. See
     https://github.com/ocaml/ocaml/issues/7074. *)
  let create_polymorphic
    (type dynamic_action static_action)
    (computation : (_, dynamic_action, static_action, r) Bonsai.Private.Computation.t)
    apply_static
    : r t
    =
    let queue = Queue.create () in
    let module A =
      Ui_effect.Define (struct
        module Action = struct
          type t = (dynamic_action, static_action) Action.t
        end

        let handle = Queue.enqueue queue
      end)
    in
    let inject = A.inject in
    let inject_dynamic a = A.inject (Dynamic a) in
    let inject_static a = A.inject (Static a) in
    let snapshot =
      Bonsai.Private.eval
        ~environment
        ~path:Bonsai.Private.Path.empty
        ~clock
        ~model:(Incr.Var.watch model_var)
        ~inject_dynamic
        ~inject_static
        computation
    in
    let result_incr = Bonsai.Private.Snapshot.result snapshot in
    let dynamic_apply_action_incr =
      Bonsai.Private.Apply_action.to_incremental
        (Bonsai.Private.Snapshot.apply_action snapshot)
    in
    let dynamic_apply_action = Incr.observe dynamic_apply_action_incr in
    let result = result_incr |> Incr.observe in
    let lifecycle_incr = Bonsai.Private.Snapshot.lifecycle_or_empty snapshot in
    let lifecycle = Incr.observe lifecycle_incr in
    Incr.stabilize ();
    T
      { model_var
      ; default_model
      ; clock
      ; inject
      ; dynamic_apply_action
      ; dynamic_apply_action_incr
      ; static_apply_action = apply_static ~inject:inject_static
      ; result
      ; result_incr
      ; lifecycle
      ; lifecycle_incr
      ; queue
      ; last_lifecycle = Bonsai.Private.Lifecycle.Collection.empty
      }
  in
  create_polymorphic component_unpacked apply_static
;;

let schedule_event _ = Ui_effect.Expert.handle

let flush (T { model_var; static_apply_action; dynamic_apply_action; queue; _ }) =
  let update_model ~action ~apply_action =
    (* The only difference between [Var.latest_value] and [Var.value] is that
       if [Var.set] is called _while stabilizing_, then calling [Var.value]
       will return the value that was set when stabilization started, whereas
       [latest_value] will give you the value that was just [set].  Now,
       setting a model in the middle of a stabilizaiton should never happen,
       but I think it's important to be explicit about which behavior we use,
       so I chose the one that would be least surprising if a stabilization
       does happen to occur. *)
    Incr.Var.set
      model_var
      (apply_action
         ~schedule_event:Ui_effect.Expert.handle
         (Incr.Var.latest_value model_var)
         action)
  in
  let process_event (action : _ Action.t) =
    match action with
    | Static action -> update_model ~apply_action:static_apply_action ~action
    | Dynamic action ->
      (* We need to stabilize before every action so that the [input] for the
         apply-actions are up to date. *)
      Incr.stabilize ();
      let apply_action = Incr.Observer.value_exn dynamic_apply_action in
      update_model ~apply_action ~action
  in
  while not (Queue.is_empty queue) do
    process_event (Queue.dequeue_exn queue)
  done;
  Incr.stabilize ()
;;

let result (T { result; _ }) = Incr.Observer.value_exn result

let trigger_lifecycles (T t) =
  let old = t.last_lifecycle in
  let new_ = t.lifecycle |> Incr.Observer.value_exn in
  t.last_lifecycle <- new_;
  schedule_event () (Bonsai.Private.Lifecycle.Collection.diff old new_)
;;

let result_incr (T { result_incr; _ }) = result_incr

let _apply_action_incr (T { dynamic_apply_action_incr; _ }) =
  Ui_incr.pack dynamic_apply_action_incr
;;

let clock (T { clock; _ }) = clock

(* maybe unnecessary *)
let _lifecycle_incr (T { lifecycle_incr; _ }) = Ui_incr.pack lifecycle_incr

let _has_after_display_events (T t) =
  let lifecycle = t.lifecycle |> Incr.Observer.value_exn in
  Bonsai.Private.Lifecycle.Collection.has_after_display lifecycle
;;

let _invalidate_observers (T { dynamic_apply_action; result; lifecycle; _ }) =
  Incr.Observer.disallow_future_use dynamic_apply_action;
  Incr.Observer.disallow_future_use result;
  Incr.Observer.disallow_future_use lifecycle
;;
