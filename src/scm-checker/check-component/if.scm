(define-library (scm-checker check-component if)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) every any remove)
          (prefix (scm-checker code-warning) w/))
  (export check-if)
  (begin
    (define (always-true-if-case? expression)
      (or (eq? (cadr expression) #t)
          (number? (cadr expression))
          (char? (cadr expression))))

    (define (always-false-if-case? expression)
      (and (eq? (cadr expression) #f)))

    (define (use-and-case? expression)
      (and (= (length expression) 4)
           (eq? (list-ref expression 3) #f)))

    (define (use-when-case? expression)
      (= (length expression) 3))

    (define (valid-if? expression)
      (and (list? expression)
           (or (= (length expression) 3)
               (= (length expression) 4))))

    (define (nest-if-case? expression)
      (and (= (length expression) 4)
           (list? (list-ref expression 3))
           (eq? (car (list-ref expression 3)) 'if)))

    (define (check-if expression debug-info)
      (cond
        ((not (valid-if? expression))
         ;;TODO: This should be error.(not warning)
          (list (w/make-code-warning debug-info "Invalid if.")))
        ((use-and-case? expression)
          (list (w/make-code-warning-with-suggestion
                  debug-info
                  "Use and."
                  expression
                  `((and ,(cadr expression)
                         ,(list-ref expression 2))))))
        ((use-when-case? expression)
          (list (w/make-code-warning-with-suggestion
                  debug-info
                  "Use when."
                  expression
                  `((when ,(cadr expression)
                          ,(list-ref expression 2))))))
        ((always-true-if-case? expression)
          (list (w/make-code-warning-with-suggestion
                  debug-info "Test is always true."
                  expression
                  `(,(list-ref expression 2)))))
        ((always-false-if-case? expression)
          (list (w/make-code-warning-with-suggestion
                  debug-info "Test is always false."
                  expression
                  `(,(list-ref expression 3)))))
        ((nest-if-case? expression)
         (list (w/make-code-warning debug-info "Use cond")))
        (else '())))))
