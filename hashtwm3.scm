;;
;; HashTWM3
;;

; Load the docstring support.
(include "docstrings.scm")

; Load the driver API
(include "drivers/api.scm")

; Have ability to break to a REPL for debugging.
; This should eventually create/destroy a console as required.
(define* c "Represents the continuation used to continue execution from the REPL." #f)
(define* (break-to-repl)
  "Break to a REPL in a temporary console window."
  (printf "~nType (c) to continue.~n")
  (call/cc
   (lambda (k)
     (set! return (lambda () (k #f)))
     (repl))))

; Do stuff.
(define* (main)
  "Main entry-point of HashTWM3."
  (driver/display-info "HashTWM3")
  (driver/display-info "Entering driver/main-loop...")
  (driver/main-loop))

(main)
