let w = 1000
let h = 600

let f =
  try float_of_string Sys.argv.(1) with
  | _ -> 1.0
;;

let fw = int_of_float (f *. float w)
let fh = int_of_float (f *. float h)

let () =
  Util.high
    ~clear_color:(0.0, 0.05, 0.1)
    ~size:(fw, fh)
    ~init:Stockview.init
    Stockview.component
;;
