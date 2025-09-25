(define-library (scm-checker check-component pair)
  (import (scheme base)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker match core) m/))
  (export check-list)
  (begin
    (define (use-make-list expression)
      (let ((pvar (m/make-pvariable (lambda (x) (not (list? x))))))
        (cond
          ((m/match `(list ,pvar ,pvar ,pvar ,pvar) expression)
           => (lambda (bindings)
                (m/construct `(make-list 4 ,pvar) bindings)))
          (else #f))))

    (define (check-list expression debug-info)
      (cond
        ((use-make-list expression)
         => (lambda (suggest)
              (list
                (w/make-code-warning-with-suggestion
                    debug-info
                    "Use make-list"
                    expression
                    (list suggest)))))
        (else '())))))
