(define-library (scm-check check-component cond)
  (import (scheme base)
          (only (srfi 1) every any remove find)
          (prefix (scm-check code-warning) w/)
          (prefix (scm-check reader) schk-rdr/))
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

    (define (simple-clause? clause)
      (and (list? clause)
           (or (else-clause? clause)
               (and (>= (length clause) 2)
                    (not (eq? (cadr clause) '=>))))))

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

    (define (check-cond code debug-info)
      (let ((resp (check-cond->case-pattern code debug-info)))
        (if resp
          (list resp)
          '())))))
