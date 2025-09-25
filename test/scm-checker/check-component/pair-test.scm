(define-library (scm-checker check-component pair-test)
  (import (scheme base)
          (prefix (scm-checker reader) reader/)
          (prefix (scm-checker code-warning) warn/)
          (prefix (scm-checker check-component pair) pair/)
          (srfi 78))
  (export test-check-component-pair)
  (begin
    (define (test-check-list)
      (check (warn/code-warning?
               (car (pair/check-list '(list 1 1 1 1) reader/<no-position>)))
             => #t)
      (let ((list-1 '(list 1)))
        (check (null?
                 (pair/check-list `(list ,list-1 ,list-1 ,list-1 ,list-1) reader/<no-position>))
               => #t)))

    (define (test-check-component-pair)
      (test-check-list))))
