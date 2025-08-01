(define-library (scm-checker checkers)
  (import (scheme base)
          (only (scheme write) write display)
          (only (srfi 1) append-map filter-map any)
          (prefix (scm-checker config) config/)
          (prefix (scm-checker adapter set) set/)
          (prefix (scm-checker reader) schk-rdr/)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker check-component import) chk-import/)
          (prefix (scm-checker check-component if) chk-if/)
          (prefix (scm-checker check-component cond) chk-cond/)
          (prefix (scm-checker check-component let) chk-let/)
          (prefix (scm-checker check-component cons) chk-cons/)
          (prefix (scm-checker check-component arithmetic) chk-arithmetic/)
          (prefix (scm-checker check-component and) chk-and/))
  (export check-code)
  (begin
    (define (handle-list code debug-info)
      (let ((debug-info-list* (schk-rdr/position-children debug-info)))
        (append-map
          (lambda (code* debug-info*) (check-code code* debug-info*))
          code
          debug-info-list*)))

    (define (import-library-declaration? expression)
      (eq? (car expression) 'import))

    (define (export-library-declaration? expression)
      (eq? (car expression) 'export))

    (define (begin-library-declaration? expression)
      (eq? (car expression) 'begin))

    (define empty-eq-set (set/make-set-eq))

    (define (get-identifiers code)
      (cond
        ((symbol? code) (set/make-set-eq))
        ((and (list? code)
              (map get-identifiers code))
         =>
         (lambda (identifiers-list)
           (if (>= (length identifiers-list) 1)
             (apply set/union identifiers-list)
             empty-eq-set)))
        (else empty-eq-set)))

    (define (filter-with-debug-info pred ls debug-infos)
      (filter-map
        (lambda (v debug-info*) (and (pred v) (cons v debug-info*)))
        ls
        debug-infos))

    (define (handle-r7rs-library code debug-info)
      (let* ((debug-infos (cddr (schk-rdr/position-children debug-info)))
             (declarations (cddr code))
             (imports (filter-with-debug-info
                        import-library-declaration?
                        declarations
                        debug-infos))
             (begins (filter-with-debug-info
                       begin-library-declaration?
                       declarations
                       debug-infos))
             (used-identifiers (get-identifiers (map car begins))))

        (append
          (if (>= (length imports) 2)
            (list (w/make-code-warning (cdr (cadr imports)) "Duplicate import."))
            '())
          (if (>= (length imports) 1)
            (append-map (lambda (x debug-info)
               (chk-import/simple-library-check x debug-info used-identifiers))
               (cdr (car (car imports)))
               (cdr (schk-rdr/position-children  (cdr (car imports)))))
            '())
          (parameterize (((config/get-dynamic-config 'srfi-1)
                          (or (any (lambda (import-set) (chk-import/used-library-namd? import-set '(srfi 1)))
                                   (cdr (caar imports)))
                              (config/get-config 'srfi-1))))
            (append-map (lambda (x debug-info*) (check-code x debug-info*))
                        (map car begins)
                        (map cdr begins))))))

    (define (check-code code debug-info)
      (cond
        ((not (list? code)) '())
        ((null? code) '())
        ((eq? (car code) 'import)
         (chk-import/check-import code debug-info) )
        ((eq? (car code) 'cond)
         (append (chk-cond/check-cond code debug-info)
                 (handle-list code debug-info)))
        ((eq? (car code) 'let)
         (append (chk-let/check-let code debug-info)
                 (handle-list code debug-info)))
        ((eq? (car code) 'define-library)
         (handle-r7rs-library code debug-info))
        ((eq? (car code) '=)
         (append (chk-arithmetic/check-= code debug-info)
                 (handle-list code debug-info)))
        ((eq? (car code) '>)
         (append (chk-arithmetic/check-> code debug-info)
                 (handle-list code debug-info)))
        ((eq? (car code) '<)
         (append (chk-arithmetic/check-< code debug-info)
                 (handle-list code debug-info)))
        ((eq? (car code) 'cons)
         (append (chk-cons/check-cons code debug-info)
                 (handle-list code debug-info)))
        ((eq? (car code) 'if)
         (append (chk-if/check-if code debug-info)
                 (handle-list code debug-info)))
        ((eq? (car code) 'and)
          (append (chk-and/check-and code debug-info)
                  (handle-list code debug-info)))
        ((eq? (car code) 'quote)
          '())
        ((list? code)
         (handle-list code debug-info))
        (else '())))))
