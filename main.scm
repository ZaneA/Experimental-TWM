;;
;; HashTWM3
;;

; Load the docstring support.
(include "docstrings.scm")

; Load the Win32 API definitions.
; Note that these aren't extensive, and only include what we use.
(include "win32-wrappers.scm")

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

; Throw a critical error, causing HashTWM3 to shutdown.
(define* (critical-error message)
  "Throw a critical error, causing HashTWM3 to break to a REPL."
  (win32/with-console
    (printf "ERROR: ~a~n" message) 
    (break-to-repl)))

; Do stuff.
(define* (main)
  "Main entry-point of HashTWM3."
  (win32/message-box "HashTWM3" "Welcome")
  (critical-error "Nothing implemented yet!"))

(main)
