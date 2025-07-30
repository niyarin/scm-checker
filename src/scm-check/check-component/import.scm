(define-library (scm-check check-component import)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) fold filter)
          (prefix (srfi 113) set/)
          (prefix (scm-check code-warning) w/)
          (prefix (scm-check reader) schk-rdr/))
  (export check-import simple-library-check )
  (begin
    (define (import-set->library-name import-set)
      (if (list? import-set)
        (case (car import-set)
          ((only except prefix rename) (import-set->library-name (cadr import-set)))
          (else import-set))
        import-set))


    (define (import-declaration->import-identifiers declaration)
      (cond
        ((eq? (car declaration) 'only)
         (cddr declaration))
        (else '())))

    (define (unused-imports->string unused-imports)
      (apply string-append
             (map (lambda (x) (string-append (symbol->string x) " "))
                  unused-imports)))

    (define (simple-library-check import-declaration debug-info used-identifiers)
      (cond
        ((eq? (car import-declaration) 'only)
         (let* ((import-identifiers (import-declaration->import-identifiers import-declaration))
                (unused-import (filter (lambda (x) (and (not (set/set-contains? used-identifiers x)) x)) import-identifiers)))
          (if (null? unused-import)
            (list)
            (list (w/make-code-warning
                    debug-info
                    (string-append "Unused import:"
                                   (unused-imports->string unused-import)))))))
        (else '())))

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
