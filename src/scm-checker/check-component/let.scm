(define-library (scm-checker check-component let)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) every filter-map fold)
          (prefix (scm-checker adapter set) set/)
          (prefix (scm-checker utils) utils/)
          (prefix (scm-checker code-warning) w/)
          (prefix (scm-checker reader) schk-rdr/))
  (export check-let check-let* )
  (begin
    (define (%binding? ls)
      (and (list? ls)
           (every (lambda (x)
                    (and (list? x)
                         (= (length x) 2)))
                  ls)))

    (define (valid-let? expression)
      (and (>= (length expression) 2)
           (or (and (symbol? (cadr expression));;named let
                    (%binding? (list-ref expression 2)))
               (%binding? (cadr expression)))))

    (define (named-let? expression)
      (symbol? (cadr expression)))

    (define (nested-let1? expression debug-info)
      (let ((bodies (if (named-let? expression)
                      (cdr (cddr expression))
                      (cddr expression)))
            (bindings (if (named-let? expression)
                        (car (cddr expression))
                        (cadr expression))))
          (and (= (length bodies) 1)
               (= (length bindings) 1)
               (not (named-let? expression))
               (list? (car bodies))
               (or (eq? (caar bodies) 'let)
                   (eq? (caar bodies) 'let*))
               (not (named-let? (car bodies))))))

    (define (combine-nested-let1 expression)
      (let* ((let2 (list-ref expression 2))
             (let2-binding (cadr let2))
             (let2-bodies (cddr let2))
             (let1-binding (cadr expression)))
        `(let*
           ,(append let1-binding let2-binding)
           ,@let2-bodies)))

    (define (%find-unused-bindings-for-let*-aux vars val-identifiers vdinfos body-identifiers)
      (let loop ((resp '())
                 (vars vars)
                 (val-identifiers val-identifiers)
                 (vdinfos vdinfos))
        (cond
          ((null? vars) resp)
          ((set/contains? body-identifiers (car vars))
           (loop resp (cdr vars) (cdr val-identifiers) (cdr vdinfos)))
          ((set/contains? (fold (lambda (x accm) (set/union x accm))
                                (set/make-set-eq)
                                (cdr val-identifiers))
                          (car vars))
           (loop resp (cdr vars) (cdr val-identifiers) (cdr vdinfos)))
          (else (loop
                  (cons (cons (car vars) (car vdinfos)) resp)
                  (cdr vars)
                  (cdr val-identifiers)
                  (cdr vdinfos))))))

    (define (find-unused-bindings-for-let* expression debug-info)
      (let* ((bindings (if (named-let? expression)
                         (list-ref expression 2)
                         (cadr expression)))
             (vdinfos
               (if (named-let? expression)
                  (schk-rdr/position-children
                    (list-ref (schk-rdr/position-children debug-info) 2))
                  (schk-rdr/position-children
                    (cadr (schk-rdr/position-children debug-info)))))
             (vars (map car bindings))
             (bind-vals (map cdr bindings))
             (bodies (if (named-let? expression)
                       (cdr (cddr expression))
                       (cddr expression)))
             (val-identifiers
               (map utils/get-identifiers bind-vals))
             (body-identifiers (utils/get-identifiers bodies)))
        (%find-unused-bindings-for-let*-aux vars val-identifiers vdinfos body-identifiers)))

    (define (find-unused-bindings expression debug-info)
      (let* ((bindings (if (named-let? expression)
                         (list-ref expression 2)
                         (cadr expression)))
             (vdinfos
               (if (named-let? expression)
                  (schk-rdr/position-children
                    (list-ref (schk-rdr/position-children debug-info) 2))
                  (schk-rdr/position-children
                    (cadr (schk-rdr/position-children debug-info)))))
             (vars (map car bindings))
             (bodies (if (named-let? expression)
                       (cdr (cddr expression))
                       (cddr expression)))
             (used-identifiers
               (utils/get-identifiers bodies))
             (unused-vars
                (filter-map
                  (lambda (v d)
                    (and (not (set/contains? used-identifiers v))
                         (cons v d)))
                  vars
                  vdinfos)))
        unused-vars))

    (define (check-let* expression debug-info)
      (cond
        ((not (valid-let? expression))
         (list (w/make-code-warning debug-info "Invalid let*.")))
        ((find-unused-bindings-for-let* expression debug-info)
         => (lambda (bs)
              (map
                (lambda (b) (w/make-code-warning (cdr b) "Unused variable."))
                bs)))
        (else '())))


    (define (check-let expression debug-info)
      (cond
        ((not (valid-let? expression))
         (list (w/make-code-warning debug-info "Invalid let.")))
        ((nested-let1? expression debug-info)
          (list (w/make-code-warning-with-suggestion
                  debug-info "Nested let."
                  expression
                  (list (combine-nested-let1 expression)))))
        ((find-unused-bindings expression debug-info)
         => (lambda (bs)
              (map
                (lambda (b)
                  (w/make-code-warning
                    (cdr b)
                    "Unused variable."))
                bs)))
        (else '())))))
