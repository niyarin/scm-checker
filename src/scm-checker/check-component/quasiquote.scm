(define-library (scm-checker check-component quasiquote)
  (import (scheme base)
          (prefix (scm-checker adapter set) set/)
          (prefix (scm-checker utils) utils/)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
  (export check-quasiquote)
  (begin
    (define (%check-use-quote-case expression)
      (let ((identifiers (utils/get-identifiers expression)))
        (and (not (set/contains? identifiers 'unquote))
             (not (set/contains? identifiers 'unquote-splicing)))))

    (define (check-quasiquote expression debug-info)
      (if (%check-use-quote-case expression)
       (list
         (w/make-code-warning-with-suggestion
           debug-info
           "Use quote."
           expression
           `(quasiquote ,(cadr expression))))
       '()))))
