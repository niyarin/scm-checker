(define-library (scm-checker check-component let)
  (import (scheme base)
          (scheme write)
          (only (srfi 1) every filter-map fold find drop append-map)
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
          ((set/contains? (fold set/union
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

    (define (find-duplicates vars debug-info-ls)
      (let loop ((vars vars)
                 (dbgs debug-info-ls)
                 (res '()))
        (cond
          ((null? vars) res)
          ((find (lambda (x) (eq? x (car vars)))
                 (cdr vars))
           (loop (cdr vars) (cdr dbgs) (cons (cons (car vars) (car dbgs)) res)))
          (else (loop (cdr vars) (cdr dbgs) res)))))

    (define (%check-argnum-loop code debug-info proc-name argnum)
      (if (and (not (null? code)) (list? code))
        (let ((res (append-map
                     (lambda (x d) (%check-argnum-loop x d proc-name argnum))
                     code
                     (schk-rdr/position-children debug-info))))
          (if (and (eq? (car code) proc-name)
                   (not (= (length (cdr code)) argnum)))
            (cons (w/make-code-warning debug-info "ERROR: Argnum is not match.")
                  res)
            res))
        '()))

    (define (check-named-let-argnum expression debug-info)
      (let ((name (cadr expression))
            (debug-infos (schk-rdr/position-children debug-info))
            (argnum (length (list-ref expression 2)))
            (bodies (drop expression 3)))
        (append-map
          (lambda (body d) (%check-argnum-loop body d name argnum))
          bodies
          (drop debug-infos 3))))

    ;;error
    (define (check-duplicated-vars expression debug-info)
      (let* ((is-named-let (named-let? expression))
             (bindings (if is-named-let
                         (list-ref expression 2)
                         (cadr expression)))
             (bindings-dbg (if is-named-let
                             (list-ref (schk-rdr/position-children debug-info) 2)
                             (cadr (schk-rdr/position-children debug-info))))
             (vars (map car bindings))
             (dbg-vars (schk-rdr/position-children bindings-dbg))
             (dups (find-duplicates (reverse vars) (reverse dbg-vars))))
        (map (lambda (var-debug)
               (w/make-code-warning
                 (cdr var-debug)
                 "Duplicate vars."))
             dups)))

    (define (check-let expression debug-info)
      (cond
        ((not (valid-let? expression))
         (list (w/make-code-warning debug-info "Invalid let.")))
        ((nested-let1? expression debug-info)
          (list (w/make-code-warning-with-suggestion
                  debug-info "Nested let."
                  expression
                  (list (combine-nested-let1 expression)))))

        ((and (named-let? expression)
              (check-named-let-argnum expression debug-info))
          => values)
        ((check-duplicated-vars  expression debug-info)
         => values)
        ((find-unused-bindings expression debug-info)
         => (lambda (bs)
              (map
                (lambda (b)
                  (w/make-code-warning
                    (cdr b)
                    "Unused variable."))
                bs)))
        (else '())))))
