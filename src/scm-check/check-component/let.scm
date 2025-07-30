(define-library (scm-check check-component let)
  (import (scheme base)
          (only (srfi 1) every)
          (prefix (scm-check code-warning) w/)
          (prefix (scm-check reader) schk-rdr/))
  (export check-let)
  (begin
    (define (%binding? ls)
      (and (list? ls)
           (every (lambda (x)
                    (and (list? x)
                         (= (length x) 2)))
                  ls)))

    (define (valid-let? expression)
      (and (>= (length expression) 2)
           (or (and (symbol? (cadr expression));;named let
                    (%binding? (list-ref expression 2)))
               (%binding? (cadr expression)))))

    (define (named-let? expression)
      (symbol? (cadr expression)))

    (define (nested-let1? expression debug-info)
      (let ((bodies (if (named-let? expression)
                      (cdr (cddr expression))
                      (cddr expression)))
            (bindings (if (named-let? expression)
                        (car (cddr expression))
                        (cadr expression))))
          (and (= (length bodies) 1)
               (= (length bindings) 1)
               (not (named-let? expression))
               (list? (car bodies))
               (or (eq? (caar bodies) 'let)
                   (eq? (caar bodies) 'let*))
               (not (named-let? (car bodies))))))

    (define (combine-nested-let1 expression)
      (let* ((let2 (list-ref expression 2))
             (let2-binding (cadr let2))
             (let2-bodies (cddr let2))
             (let1-binding (cadr expression)))
        `(let*
           ,(append let1-binding let2-binding)
           ,@let2-bodies)))

    (define (check-let expression debug-info)
      (cond
        ((not (valid-let? expression))
         (list (w/make-code-warning debug-info "Invalid let.")))
        ((nested-let1? expression debug-info)
          (list (w/make-code-warning-with-suggestion
                  debug-info "Nested let."
                  expression
                  (list (combine-nested-let1 expression)))))
        (else '())))))
