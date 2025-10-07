(define-library (scm-checker check-component and)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) every)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
  (export check-and)
  (begin
    (define (one-element-and? expression)
      (and (list? expression)
           (not (null? (cdr expression)))
           (null? (cddr expression))))


    (define (and-expression? expression)
      (and (list? expression)
           (eq? (car expression) 'and)))

    (define (find-nested-and expression debug-infos)
      (let loop ((args (cdr expression))
                 (debug-infos (cdr debug-infos)))
        (cond
          ((null? args) #f)
          ((and-expression? (car args))
           (cons (car args) (car debug-infos)) )
          (else (loop (cdr args) (cdr debug-infos))))))

    (define (combinable-equal-procedure? x)
     (case x
       ((char=? =) #t)
       (else #f)))

    (define (find-combinedable-compares expression)
      (and (every (lambda (x) (and (list? x)
                                   (eq? (car (cadr expression)) (car x))))
                  (cdr expression))
           (combinable-equal-procedure? (car (cadr expression)))
           (every (lambda (x) (eq? (list-ref (cadr expression) 2)
                                   (list-ref x 2)))
                  (cdr expression))))

    (define (check-and expression debug-info)
      (cond
        ((one-element-and? expression)
         (list (w/make-code-warning-with-suggestion
                 debug-info "Unnecessary and."
                 expression
                 (list (cadr expression)))))
        ((find-nested-and expression (schk-rdr/position-children debug-info))
         => (lambda (pair-of-args-pos)
              (list (w/make-code-warning-with-suggestion
                     (cdr pair-of-args-pos)
                     "Nested and."
                     (car pair-of-args-pos)
                     (cdr (car pair-of-args-pos))))))
        ((find-combinedable-compares expression)
         (list
           (w/make-code-warning-with-suggestion
             debug-info
              "Equivalent formula that can be combined into one."
              expression
              (list
                 (append
                   (list (car (cadr expression)))
                   (map cadr (cdr expression))
                   (list (list-ref (cadr expression) 2)))))))
        (else '())))))
