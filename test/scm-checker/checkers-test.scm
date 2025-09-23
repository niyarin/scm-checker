(define-library (scm-checker checkers-test)
  (import (scheme base)
          (scheme write)
          (prefix (scm-checker checkers) checkers/)
          (srfi 78))
  (export test-checkers)
  (begin
    (define (test-handle-list)
      (check (checkers/check-code "test" '()) => '()))

    (define (test-checkers)
      (test-handle-list)

      )))
