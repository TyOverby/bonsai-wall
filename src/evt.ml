open Core

type t =
  | Mouse_button_down of int
  | Mouse_button_up of int
  | Mouse_motion of
      { x : int
      ; y : int
      ; x_rel : int
      ; y_rel : int
      }
  | Mouse_wheel of
      { which : int
      ; x : int
      ; y : int
      }
[@@deriving sexp_of]
