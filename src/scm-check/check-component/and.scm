(define-library (scm-check check-component and)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) every any remove)
          (prefix (scm-check code-warning) w/)
          (prefix (scm-check reader) schk-rdr/))
  (export check-and)
  (begin
    (define (one-element-and? expression)
      (and (list? expression)
           (not (null? (cdr expression)))
           (null? (cddr expression))))

    (define (check-and expression debug-info)
      (cond
        ((one-element-and? expression)
         (list (w/make-code-warning-with-suggestion
                 debug-info "Unnecessary and."
                 expression
                 (list (cadr expression)))))
        (else '())))))
