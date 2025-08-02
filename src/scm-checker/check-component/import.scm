(define-library (scm-checker check-component import)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) fold filter)
          (prefix (scm-checker adapter set) set/)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
  (export check-import simple-library-check )
  (begin
    (define (import-set->library-name import-set)
      (if (list? import-set)
        (case (car import-set)
          ((only except prefix rename) (import-set->library-name (cadr import-set)))
          (else import-set))
        import-set))


    (define (standard-library-name->identifiers library-name)
      (if (not (= (length library-name) 2))
        (error "Error.")
        (case (cadr library-name)
          ((write) '(write display write-shared write-simple))
          (else '()))))



    (define (import-declaration->import-identifiers declaration)
      ;;TODO: WIP
      (cond
        ((eq? (car declaration) 'only)
         (cddr declaration))
        ((eq? (car declaration) 'prefix)
         '())
        ((and (eq? (car declaration) 'scheme))
          (standard-library-name->identifiers declaration))
        (else '())))

    (define (unused-imports->string unused-imports)
      (apply string-append
             (map (lambda (x) (string-append (symbol->string x) " "))
                  unused-imports)))

    (define (any-used-identifiers? import-clause used-identifiers)
      ;;TODO: WIP
      (let* ((import-identifiers
              (import-declaration->import-identifiers import-clause))
             (used (set/intersection (set/list->eq-set import-identifiers)
                                         used-identifiers)))

        (and (not (null? import-identifiers))
             (not (set/empty? used))
             used)))


    (define (simple-library-check import-declaration debug-info used-identifiers)
      (cond
        ((eq? (car import-declaration) 'only)
         (let* ((import-identifiers (import-declaration->import-identifiers import-declaration))
                (unused-import (filter (lambda (x) (and (not (set/contains? used-identifiers x)) x)) import-identifiers)))
          (if (null? unused-import)
            (list)
            (list (w/make-code-warning
                    debug-info
                    (string-append "Unused import:"
                                   (unused-imports->string unused-import)))))))
        ((not (or (eq? (car import-declaration) 'except)
                  (eq? (car import-declaration) 'prefix)
                  (eq? (car import-declaration) 'rename)))
         (if (and (equal? import-declaration
                          '(scheme write))
                 (not (any-used-identifiers?  import-declaration used-identifiers)))
           (list
             (w/make-code-warning-with-suggestion
               debug-info
               "Unused library."
               import-declaration
               '()))
           '()))
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
