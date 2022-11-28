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

type enum =
  [ `App_did_enter_background
  | `App_did_enter_foreground
  | `App_low_memory
  | `App_terminating
  | `App_will_enter_background
  | `App_will_enter_foreground
  | `Audio_device_added
  | `Audio_device_removed
  | `Clipboard_update
  | `Controller_axis_motion
  | `Controller_button_down
  | `Controller_button_up
  | `Controller_device_added
  | `Controller_device_remapped
  | `Controller_device_removed
  | `Dollar_gesture
  | `Dollar_record
  | `Drop_begin
  | `Drop_complete
  | `Drop_file
  | `Drop_text
  | `Finger_down
  | `Finger_motion
  | `Finger_up
  | `Keymap_changed
  | `Joy_axis_motion
  | `Joy_ball_motion
  | `Joy_button_down
  | `Joy_button_up
  | `Joy_device_added
  | `Joy_device_removed
  | `Joy_hat_motion
  | `Key_down
  | `Key_up
  | `Mouse_button_down
  | `Mouse_button_up
  | `Mouse_motion
  | `Mouse_wheel
  | `Multi_gesture
  | `Quit
  | `Render_targets_reset
  | `Render_device_reset
  | `Sys_wm_event
  | `Text_editing
  | `Text_input
  | `Unknown of int
  | `User_event
  | `Window_event
  | `Display_event
  | `Sensor_update
  ]
[@@deriving sexp_of]
