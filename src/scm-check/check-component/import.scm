(define-library (scm-check check-component import)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) fold)
          (prefix (scm-check code-warning) w/)
          (prefix (scm-check reader) schk-rdr/))
  (export check-import)
  (begin
    (define (import-set->library-name import-set)
      (if (list? import-set)
        (case (car import-set)
          ((only except prefix rename) (import-set->library-name (cadr import-set)))
          (else import-set))
        import-set))

    (define (check-import* code debug-info)
      (fold
        (lambda (code* debug-info* accum)
          (let* ((library-name (import-set->library-name code*))
                 (appended-import
                  (cons library-name (car accum))))
            (if (member library-name (car accum))
              (list appended-import
                    (cons (w/make-code-warning-with-suggestion
                            debug-info* "Duplicate import." (car accum) '())
                          (cadr accum)))
              (list appended-import
                    (cadr accum)))))
        (list '();;used librarynames
              '());;error
        (cdr code)
        (cdr (schk-rdr/position-children debug-info))))

    (define (check-import code debug-info)
      (cadr (check-import* code debug-info)))))
