open! Base

val high
  :  ?clear_color:float * float * float
  -> ?scale:float
  -> size:int * int
  -> init:(context:Wall.renderer -> 'a)
  -> (mouse:(float * float) Bonsai.Value.t
      -> size:(float * float) Bonsai.Value.t
      -> 'a
      -> (Wall.image * (Evt.t -> unit Bonsai.Effect.t)) Bonsai.Computation.t)
  -> unit
