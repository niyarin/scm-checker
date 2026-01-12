(define-library (scm-checker check-component list)
  (import (scheme base)
          (prefix (scm-checker match core) m/)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
  (export check-list-for-=
          check-list-for->)
  (begin
    (define (%length-one-check expression debug-info)
      (cond
        ((m/match `(= (length ,m/var1) 1) expression)
         => (lambda (bindings)
              (w/make-code-warning-with-suggestion
                debug-info
                "Causes unnecessary traversals."
                expression
                (list (m/construct `(and (pair? ,m/var1)
                                         (null? (cdr ,m/var1)))
                                   bindings)))))
        (else #f)))

    (define (%length-zero-check expression debug-info)
      (cond
        ((or (m/match `(= (length ,m/var1) 0) expression)
             (m/match `(zero? (length ,m/var1)) expression))
         => (lambda (bindings)
              (w/make-code-warning-with-suggestion
                debug-info
                "Causes unnecessary traversals."
                expression
                (list (m/construct `(null? (cdr ,m/var1))
                                   bindings)))))
        (else #f)))

    (define (%length-more-than-one-check expression debug-info)
      (cond
        ((m/match `(> (length ,m/var1) 1) expression)
         => (lambda (bindings)
              (w/make-code-warning-with-suggestion
                debug-info
                "Causes unnecessary traversals."
                expression
                (list (m/construct `(and (pair? ,m/var1)
                                         (pair? (cdr ,m/var1)))
                                   bindings)))))
        (else #f)))

    ;;Detect like (= (length some-list) 1) => (null? (cdr some-list))
    (define (check-list-for-= expression debug-info)
      (cond
        ((%length-one-check expression debug-info)
         => list)
        ((%length-zero-check expression debug-info)
         => list)
        (else '())))

    (define (check-list-for-> expression debug-info)
      (cond
        ((%length-more-than-one-check expression debug-info)
         => list)
        (else '())))
    ))
