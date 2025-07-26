(define-library (scm-check check-component if)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) every any remove)
          (prefix (scm-check code-warning) w/)
          (prefix (scm-check reader) schk-rdr/))
  (export check-if)
  (begin
    (define (use-and-case? expression)
      (and (= (length expression) 4)
           (eq? (list-ref expression 3) #f)))

    (define (check-if expression debug-info)
      (cond
        ((use-and-case? expression)
          (list (w/make-code-warning debug-info "Use and.")))
        (else
          '())))))
