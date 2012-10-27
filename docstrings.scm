;;;
;;; Docstrings support
;;;

(define-for-syntax *documentation-hash-table* (make-hash-table))

(define-syntax (define* form rename compare)
  (let* ((args-form (second form))
         (proc-name (first args-form))
         (doc (third form))
         (body (drop form 3))
         (define% (rename 'define)))
    (hash-table-set! *documentation-hash-table* proc-name (list doc body)) 
    `(,define% ,args-form ,@body)))

(define* (help)
  "Print a list of documented procedures."
  (hash-table-walk *documentation-hash-table*
    (lambda (key val)
      (printf "~a => ~a~n ~a~n~n" key (first val) (second val))))) 
