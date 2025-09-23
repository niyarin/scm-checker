(import (scheme base)
        (scheme process-context)
        (scheme write)
        (srfi 78)
        (scm-checker checkers-test))

(define test-defs
  `(("scm-checker.checkers-test" .  ,test-checkers)))

(define (do-test tests)
  (for-each
    (lambda (f) (f))
    tests))

(define (test* args)
  (if (null? args)
    (do-test (map cdr test-defs))
    (do-test (list
               (cond ((assoc (car args) test-defs) => cdr)
                     (else values))))))

(define (test)
  (test* (cdr (command-line)))
  (check-report))

(test)
