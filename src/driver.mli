open! Base
open! Bonsai
module Incr := Ui_incr

type 'r t

val create : ?clock:Incr.Clock.t -> 'r Computation.t -> 'r t
val schedule_event : _ t -> unit Ui_effect.t -> unit
val flush : _ t -> unit
val trigger_lifecycles : _ t -> unit
val clock : _ t -> Incr.Clock.t
val result : 'r t -> 'r
val result_incr : 'r t -> 'r Incr.t
