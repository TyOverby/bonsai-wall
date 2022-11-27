open! Base

val low
  :  size:int * int
  -> init:(context:Wall.renderer -> 'a)
  -> after_display:('a -> unit)
  -> f:(context:Wall.renderer -> sw:float -> sh:float -> 'a -> unit)
  -> unit

val mid
  :  ?scale:float
  -> size:int * int
  -> init:(context:Wall.renderer -> 'a)
  -> after_display:('a -> unit)
  -> f:(mouse:float * float -> size:float * float -> 'a -> Wall.image)
  -> unit
  -> unit

val high
  :  ?scale:float
  -> size:int * int
  -> init:(context:Wall.renderer -> 'a)
  -> (mouse:(float * float) Bonsai.Value.t
      -> size:(float * float) Bonsai.Value.t
      -> 'a
      -> Wall.image Bonsai.Computation.t)
  -> unit
