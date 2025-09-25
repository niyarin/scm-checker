(define-library (scm-checker check-component cond)
  (import (scheme base)
          (only (srfi 1) every any remove find last)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
  (export check-cond)
  (begin
    (define (else-clause? clause)
      (eq? (car clause) 'else))

    (define (quote-symbol-expression? expression)
      (and (list? expression)
           (eq? (car expression) 'quote)
           (symbol? (cadr expression))))

    (define (atom-except-identifier? object)
      (or (integer? object)
          (char? object)
          (boolean? object)))

    ;;TODO: Support non symbol data.
    (define (check-atom-eq-clause clause)
      (let* ((test (car clause))
             (test-ope (and (list? test) (car test))))
        (or (and (eq? test-ope 'zero?)
                 (list (cadr test) 0 (cadr clause)))
            (and test-ope
                 (or (eq? test-ope 'eq?)
                     (eq? test-ope 'eqv?)
                     (eq? test-ope '=)
                     (eq? test-ope 'boolean=?)
                     (eq? test-ope 'char=?))
                 (or (and (or (quote-symbol-expression? (cadr test))
                              (atom-except-identifier? (cadr test)))
                          (list (list-ref test 2)
                                (if (quote-symbol-expression? (cadr test))
                                  (cadr (cadr test))
                                  (cadr test))
                                (cadr clause)))
                     (and (or (quote-symbol-expression? (list-ref test 2))
                              (atom-except-identifier? (list-ref test 2)))
                          (list (cadr test)
                                (if (quote-symbol-expression? (list-ref test 2))
                                  (cadr (list-ref test 2))
                                  (list-ref test 2))
                                (cadr clause))))))))

    (define (apply-clause? clause)
      (and (= (length clause) 3)
           (eq? (cadr clause) '=>)))

    (define (simple-clause? clause)
      (and (list? clause)
           (or (else-clause? clause)
               (and (>= (length clause) 2)
                    (not (apply-clause? clause))))))

    (define (all-same? ls)
      (or (null? ls)
          (null? (cdr ls))
          (let loop ((ls* (cdr ls)))
            (cond
              ((null? ls*) #t)
              ((equal? (car ls*) (car ls))
               (loop (cdr ls*)))
              (else #f)))))

    (define (make-case-expression eq-clauses else-expression)
      `((case (,(caar eq-clauses))
        ,@(map (lambda (x) (list (list (cadr x))
                                 (list-ref x 2)))
              eq-clauses)
        ,@(if else-expression (list else-expression) '()))))

    (define (check-cond->case-pattern code debug-info)
      (let* ((clauses (cdr code)))
        (cond
          ((and (every simple-clause? clauses)
                (map check-atom-eq-clause
                     (remove else-clause? clauses)))
           => (lambda (eq-clauses)
                (and (not (any (lambda (x) (eq? x #f)) eq-clauses))
                     (all-same? (map car eq-clauses))
                     (w/make-code-warning-with-suggestion
                        debug-info "Use case."
                        code
                        (make-case-expression
                          eq-clauses
                          (find else-clause? clauses))))))
          (else #f))))

    (define (%tail2 ls)
      (let loop ((res1 #f)
                 (res2 #f)
                 (reverse-head '())
                 (ls ls))
        (if (null? ls)
          (values res1 res2
                  (cond
                    ((null? reverse-head) reverse-head)
                    ((null? (cdr reverse-head)) (cdr reverse-head))
                    (else (cddr reverse-head))))
          (loop res2
                (car ls)
                (cons (car ls) reverse-head)
                (cdr ls)))))

    (define (check-merged-boolean-pattern code)
      (let-values (((clause1 clause2 reverse-head) (%tail2 code)))
        (cond
          ((not
             (and (else-clause? clause2)
                  (= (length clause1) 2)
                  (= (length clause2) 2)))
           #f)
          ((and (eq? (cadr clause1) #t)
                (eq? (cadr clause2) #f))
           (reverse (cons `(else ,(car clause1)) reverse-head)))
          ((and (eq? (cadr clause1) #f)
                (eq? (cadr clause2) #t))
           (reverse (cons `(else (not ,(car clause1))) reverse-head)))
          (else #f))))

    (define (cond-may-returns-unspecified-value? code debug-info)
      (and (not (any else-clause? (cdr code)))
           (not (apply-clause? (last code)))))

    (define (check-invalid-cond code debug-info)
      (cond
        ((null? (cdr code))
         (w/make-code-warning debug-info "Invalid cond."))
        (else #f)))

    (define (check-cond code debug-info)
      (cond
        ((check-invalid-cond code debug-info) => list)
        ((check-cond->case-pattern code debug-info) => list)
        ((cond-may-returns-unspecified-value? code debug-info)
         (list
          (w/make-code-warning
            debug-info
            "Cond may returns an unspecified value.")))
        ((check-merged-boolean-pattern code)
         => (lambda (suggested-code)
              (list
                (w/make-code-warning-with-suggestion
                          debug-info "Contain mergeable clauses."
                          code
                          (list suggested-code)))))
        (else '())))))
