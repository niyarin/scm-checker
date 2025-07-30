(define-library (scm-check common)
  (import (scheme base)
          (prefix (srfi 128) comparator/))
  (export eq-comparator)
  (begin
    (define eq-comparator (comparator/make-eq-comparator))))
