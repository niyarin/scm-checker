(define-library (scm-checker check-component arithmetic)
  (import (scheme base)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker match core) m/)
          (prefix (scm-checker reader) schk-rdr/))
  (export check-= check-> check-<)
  (begin
    (define (check-use-zero-case expression)
      (cond
        ((not (= (length expression) 3)) #f)
        ((eqv? (list-ref expression 2) 0) (cadr expression))
        ((eqv? (cadr expression) 0) (list-ref expression 2))
        (else #f)))

    (define (check-use-positive?-case expression)
      (cond
        ((not (= (length expression) 3)) #f)
        ((eqv? (list-ref expression 2) 0)
         (cadr expression))
        (else #f)))

    (define (check-use-negative?-case expression)
      (cond
        ((not (= (length expression) 3)) #f)
        ((eqv? (list-ref expression 2) 0)
         (cadr expression))
        (else #f)))

   (define (check-use-odd? expression debug-info)
     (cond
       ((m/match `(= (modulo ,m/var1 2) 1) expression)
        =>
        (lambda (bindings)
          (w/make-code-warning-with-suggestion
                     debug-info "Use odd?."
                     expression
                     (list (m/construct `(odd? ,m/var1)
                                        bindings)))))
       (else #f)))

    (define (check-= expression debug-info)
      (cond
        ((check-use-zero-case expression)
         => (lambda (v)
              (list (w/make-code-warning-with-suggestion
                      debug-info "Use zero?."
                      expression `((zero? ,v))))))
        ((check-use-odd? expression debug-info)
         => (lambda (v) (list v)))
        (else '())))

    (define (check-> expression debug-info)
      (cond
        ((check-use-positive?-case expression)
         => (lambda (v)
              (list (w/make-code-warning-with-suggestion
                      debug-info "Use positive?."
                      expression `((positive? ,v))))))
        (else '())))

    (define (check-< expression debug-info)
      (cond
        ((check-use-negative?-case expression)
         => (lambda (v)
              (list (w/make-code-warning-with-suggestion
                      debug-info "Use negative?"
                      expression `((negative? ,v))))))
        (else '())))))
