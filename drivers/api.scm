;;;
;;; Driver API
;;; Defines a common API into each windowing system.
;;;

(define* driver/display-info
  "Display an info message."
  (foreign-lambda void "driver_display_info" c-string))

(define* driver/display-error
  "Display an error message."
  (foreign-lambda void "driver_display_error" c-string))

(define* driver/get-last-error
  "Get last error from driver."
  (foreign-lambda c-string "driver_get_last_error"))

(define* driver/get-window-title
  "Get the window title from the provided handle."
  (foreign-lambda c-string "driver_get_window_title" c-pointer))

(define* driver/get-window-class
  "Get the window class from the provided handle."
  (foreign-lambda c-string "driver_get_window_class" c-pointer))

(define* driver/main-loop
  "Driver main loop."
  (foreign-lambda void "driver_main_loop"))
