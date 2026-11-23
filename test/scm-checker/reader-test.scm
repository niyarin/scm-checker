(define-library (scm-checker reader-test)
  (import (scheme base)
          (srfi 78)
          (prefix (scm-checker reader) reader/))
  (export test-bracket test-reader)
  (begin
    (define (test-bracket)
      (check
        (caar
          (reader/read-list1 (open-input-string "(let [[a 1] [b 2]] (cons a b))")
                             reader/<no-position>))
        => '(let ((a 1) (b 2)) (cons a b))))

    (define (test-reader)
      (test-bracket))))

