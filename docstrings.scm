;;;
;;; Docstrings support
;;;

(use srfi-69)

(define-for-syntax *documentation-hash-table* (make-hash-table))

(define-syntax (define* form rename compare)
  (let* ((args-form (second form))
         (proc-name (if (list? args-form)
                      (first args-form)
                      args-form))
         (doc (third form))
         (body (drop form 3))
         (define% (rename 'define)))
    (hash-table-set! *documentation-hash-table* proc-name (list doc args-form body)) 
    `(,define% ,args-form ,@body)))

(define* (help)
  "Print a list of documented procedures."
  (hash-table-walk *documentation-hash-table*
    (lambda (key val)
      (let ((doc (first val))
            (args (second val))
            (body (third val))) 
        (printf "~a => ~a~n ~a~n~n" key doc body)))))
