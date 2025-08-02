(define-library (scm-checker utils)
  (import (scheme base)
          (prefix (scm-checker adapter set) set/))
  (export get-identifiers)
  (begin
    (define empty-eq-set (set/make-set-eq))

    (define (get-identifiers code)
      (cond
        ((symbol? code)
         (set/make-set-eq code))
        ((and (list? code)
              (map get-identifiers code))
         =>
         (lambda (identifiers-list)
           (if (>= (length identifiers-list) 1)
             (apply set/union identifiers-list)
             empty-eq-set)))
        (else empty-eq-set)))))
