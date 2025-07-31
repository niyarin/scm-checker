(define-library (scm-check check-component arithmetic)
  (import (scheme base)
          (prefix (scm-check code-warning) w/)
          (prefix (scm-check reader) schk-rdr/))
  (export check-=)
  (begin
    (define (check-use-zero-case expression)
      (cond
        ((not (= (length expression) 3)) #f)
        ((eqv? (list-ref expression 2) 0) (cadr expression))
        ((eqv? (cadr expression) 0) (list-ref expression 2))
        (else #f)))


    (define (check-= expression debug-info)
      (cond
        ((check-use-zero-case expression)
         => (lambda (v)
              (list (w/make-code-warning-with-suggestion
                      debug-info "User zero?."
                      expression `((zero? ,v))))))
        (else '())))))
