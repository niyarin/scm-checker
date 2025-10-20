(define-library (scm-checker config)
  (import (scheme base))
  (export *dynamic-configs* get-config get-dynamic-config
          set-initial-config!)
  (begin
    (define *dynamic-configs*
      (list (cons 'srfi-1 (make-parameter #f))))

    (define (set-initial-config! feature value)
      (cond
        ((assq feature *dynamic-configs*)
         => (lambda (apair)
              (set-cdr! apair (make-parameter value))))
        (else (error "Undefined feature"))))

    (define (get-dynamic-config feature)
      (cond
        ((assq feature *dynamic-configs*)
         => cdr)
        (else (error "Undefined feature."))))

    (define (get-config feature)
      ((get-dynamic-config feature)))))
