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

; This is used in win32-wrappers.scm for FailureCB.
(define* (failure-callback message)
  "Failure callback, we are about to close but can break to REPL first."
  (win32/with-console
    (let ((msg (format "ERROR: ~a; error code ~a~n" message (win32/get-last-error))))
      (display msg)
      (win32/message-box msg))
    (break-to-repl)))

; Do stuff.
(define* (main)
  "Main entry-point of HashTWM3."
  (win32/create-message-window)
  (win32/main-loop))

(main)
