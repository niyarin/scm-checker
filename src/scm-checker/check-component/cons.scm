(define-library (scm-checker check-component cons)
  (import (scheme base)
          (scheme write)
          (prefix (scm-checker config) config/)
          (prefix (scm-checker match core) m/)
          (only (srfi 1) every any remove find)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
   (export check-cons check-cons-in-the-lambda )
  (begin
    (define (use-cons*-case expression)
      (if (and (list? (list-ref expression 2))
               (eq? (car (list-ref expression 2)) 'cons))
        (list (cadr expression)
              (cadr (list-ref expression 2))
              (list-ref (list-ref expression 2) 2))
        #f))

    (define (use-xcons-case expression debug-info)
      (cond
        ((m/match `(lambda (,m/var1 ,m/var2)
                     (cons ,m/var2 ,m/var1))
                  expression)
         => (lambda (bindings)
              (w/make-code-warning-with-suggestion
                debug-info
                "Use xcons"
                expression
                (list (m/construct `(xcons ,m/var1 ,m/var2)
                                   bindings)))))
        (else #f)))

    (define (check-cons-in-the-lambda expression debug-info)
      (cond
        ((and (config/get-config 'srfi-1)
              (use-xcons-case  expression debug-info))
         => list)
        (else '())))

    (define (check-cons expression debug-info)
      (cond
        ((and (config/get-config 'srfi-1)
              (use-cons*-case expression))
         => (lambda (suggested-args)
              (list
                (w/make-code-warning-with-suggestion
                  debug-info
                  "Use cons*"
                  expression
                  (cons 'cons* suggested-args)))))
        (else '())))))
