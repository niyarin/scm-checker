(define-library (scm-checker config)
  (import (scheme base))
  (export *dynamic-configs* get-config get-dynamic-config)
  (begin
    (define *dynamic-configs*
      (list (cons 'srfi-1 (make-parameter #f))))

    (define (get-dynamic-config feature)
      (cond
        ((assq feature *dynamic-configs*)
         => cdr)
        (else (error "Undefined feature."))))

    (define (get-config feature)
      ((get-dynamic-config feature)))))
