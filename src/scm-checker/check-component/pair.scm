(define-library (scm-checker check-component pair)
  (import (scheme base)
          (prefix (scm-checker config) config/)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker match core) m/))
  (export check-list check-cxr)
  (begin
    (define (use-make-list expression)
      (let ((pvar (m/make-pvariable (lambda (x) (not (list? x))))))
        (cond
          ((m/match `(list ,pvar ,pvar ,pvar ,pvar) expression)
           => (lambda (bindings)
                (m/construct `(make-list 4 ,pvar) bindings)))
          (else #f))))

    (define (%check-cxr-srfi-1 expression debug-info)
      (cond
        ;;drop 3
        ((or (m/match `(cdddr ,m/var1) expression)
             (m/match `(cdr (cddr ,m/var1)) expression)
             (m/match `(cddr (cdr ,m/var1)) expression))
         => (lambda (bindings)
              (list
                (w/make-code-warning-with-suggestion
                  debug-info
                  "Use drop 3."
                  expression
                  (list (m/construct `(drop ,m/var1 3) bindings))))))

        ;;drop 4
        ((or (m/match `(cddddr ,m/var1) expression)
             (m/match `(cddr (cddr ,m/var1)) expression))
         => (lambda (bindings)
              (list
                (w/make-code-warning-with-suggestion
                  debug-info
                  "Use drop 4."
                  expression
                  (list (m/construct `(drop ,m/var1 4) bindings))))))

        (else  '())))

    (define (check-cxr expression debug-info)
      (cond
        ((config/get-config 'srfi-1)
         (%check-cxr-srfi-1 expression debug-info))
        (else '())))

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
