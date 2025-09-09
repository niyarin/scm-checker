(define-library (scm-checker check-component lambda)
  (import (scheme base)
          (prefix (scm-checker config) config/)
          (prefix (scm-checker match core) m/)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
   (export check-lambda)
   (begin
    (define (check-unnecessary-lambda expression debug-info)
      (cond
        ((m/match `(lambda ,m/var1 (,m/var2 . ,m/var1)) expression)
         => (lambda (bindings)
              (w/make-code-warning-with-suggestion
                debug-info
                "Unnecessary lambda."
                expression
                (list (m/construct `(,m/var2 . ,m/var1)
                                   bindings)))))
        (else #f)))

    (define (check-lambda expression debug-info)
      (cond
        ((check-unnecessary-lambda expression debug-info) => list)
        (else '())))))
