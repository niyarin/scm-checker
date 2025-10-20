(define-library (scm-checker check-component define)
  (import (scheme base)
          (only (srfi 1) filter-map)
          (prefix (scm-checker adapter set) set/)
          (prefix (scm-checker utils) utils/)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
  (export check-define )
  (begin

    (define (valid-define? expression)
      (and (>= (length expression) 2)
           (or (symbol? (cadr expression))
               (and (pair? (cadr expression))
                    (symbol? (car (cadr expression)))))))

    (define (function-define? expression)
      (list? (cadr expression)))

    (define (find-unused-args expression debug-info)
      (let* ((vars (cdr (cadr expression)))
             (arg-infos
               (cdr (schk-rdr/position-children
                       (cadr (schk-rdr/position-children debug-info)))))
             (bodies (cddr expression))
             (used-identifiers
               (utils/get-identifiers bodies))
             (unused-vars
                (filter-map
                  (lambda (v d)
                    (and (not (set/contains? used-identifiers v))
                         (cons v d)))
                  vars
                  arg-infos)))
        unused-vars))

    (define (check-define expression debug-info)
      (cond
        ((not (valid-define? expression))
         (list (w/make-code-warning debug-info "Invalid define")))
        ((and (function-define? expression)
              (find-unused-args expression debug-info))
         => (lambda (bs)
              (map
                (lambda (b)
                  (w/make-code-warning
                    (cdr b)
                    "Unused variable."))
                bs)))
        (else '())))))
