(define-library (scm-check check-component cond)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) every any remove)
          (prefix (scm-check code-warning) w/)
          (prefix (scm-check reader) schk-rdr/))
  (export check-cond)
  (begin
    (define (else-clause? clause)
      (eq? (car clause) 'else))

    (define (quote-expression? expression)
      (and (list? expression)
           (eq? (car expression) 'quote)))

    ;;TODO: Support non symbol data.
    (define (check-symbol-eq-clause clause)
      (let* ((test (car clause))
             (test-ope (car test)))
        (and (or (eq? test-ope 'eq?)
                 (eq? test-ope 'eqv?)
                 (eq? test-ope 'equal?))
             (or (and (quote-expression? (cadr test))
                      (symbol? (list-ref test 2))
                      (cons (list-ref test 2) (cadr (cadr test))))
                 (and (quote-expression? (list-ref test 2))
                      (symbol? (cadr test))
                      (cons (cadr test) (cadr (list-ref test 2))))))))

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
              ((eq? (car ls*) (car ls))
               (loop (cdr ls*)))
              (else #f)))))

    (define (check-cond->case-pattern code debug-info)
      (let* ((clauses (cdr code)))
        (if (every simple-clause? clauses)
          (let ((check-res (map check-symbol-eq-clause
                                 (remove else-clause? clauses))))
            (if (and (not (any (lambda (x) (eq? x #f)) check-res))
                     (all-same? (map car check-res)))
              (w/make-code-warning debug-info
                                   "Use case.")
              #f))
          #f)))

    (define (check-cond code debug-info)
      (let ((resp (check-cond->case-pattern code debug-info)))
        (if resp
          (list resp)
          '())))))
