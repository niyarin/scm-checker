(define-library (scm-checker match core)
  (import (scheme base)
          (only (srfi 1) fold)
          (prefix (scm-checker reader)
                  schk-rdr/)
          (prefix (scm-checker adapter box) box/))
  (export make-variable match make-bindings construct
          ref-bindings
          var1 var2 var3 var4
          make-pvariable)
  (begin
    ;;TODO: Support ellipsis pattern.

    (define-record-type <variable>
      (%make-variable name)
      variable?
      (name variable->name))

    (define-record-type <with-pred-variable>
      (%make-pvariable name pred)
      pvariable?
      (name pvariable->name)
      (pred pvariable->pred))

    (define (bindings->matched bindings)
      (let-values (((matched _) (box/unbox bindings)))
        matched))

    (define (bindings->debug-info bindings)
      (let-values (((_ debug-info) (box/unbox bindings)))
        debug-info))

    (define *cnt* 0)
    (define (make-variable)
      (set! *cnt* (+ *cnt* 1))
      (%make-variable (string->symbol
                        (string-append "v" (number->string *cnt*)))))

    (define (make-pvariable pred)
      (set! *cnt* (+ *cnt* 1))
      (%make-pvariable (string->symbol
                        (string-append "v" (number->string *cnt*)))
                       pred))


    (define var1 (make-variable))
    (define var2 (make-variable))
    (define var3 (make-variable))
    (define var4 (make-variable))

    (define (make-bindings)
      (box/box '() '()))

    (define (check-and-bind var val debug-info bindings)
      (let ((exists-cell (assq var (bindings->matched bindings))))
        (cond
          ((not exists-cell)
            (box/set-box! bindings
                                 (cons (cons var val) (bindings->matched bindings))
                                 (cons (cons var debug-info) (bindings->debug-info bindings)))
             #t)
          ((equal? val (cdr exists-cell))
           (box/set-box! bindings
                         (bindings->matched bindings)
                         (cons (cons var debug-info) (bindings->debug-info bindings)))
           #t)
          (else #f))))

    (define (%match language input debug-info bindings)
      (cond
        ((variable? language)
         (check-and-bind language input debug-info bindings))
        ((pvariable? language)
         (and ((pvariable->pred language) input)
              (check-and-bind language input debug-info bindings)))
        ((string? language)
         (and (string? input)
              (string=? input language)))
        ((list? language)
         (and (list? input)
              (= (length language) (length input))
              (fold (lambda (v1 v2 d accm)
                      (and accm (%match v1 v2 d bindings)))
                    #t
                    language
                    input
                    (if debug-info
                      (schk-rdr/position-children debug-info)
                      (map (lambda (x) #f) input)))))
        ((pair? language)
         (and (pair? input)
              (%match (car language) (car input)
                      (and debug-info
                           (car (schk-rdr/position-children debug-info)))
                      bindings)
              (%match (cdr language) (cdr input)
                      (and debug-info
                           (cdr (schk-rdr/position-children debug-info)))
                      bindings)))
        (else (eqv? language input))))

    (define (match language input . opt)
      ;;TODO: Use case lambda
      (let ((bindings (if (null? opt) (make-bindings) (car opt))))
        (and (%match language input #f bindings)
             bindings)))

    (define (match-with-debug-info language input debug-info . opt)
      (let ((bindings (if (null? opt) (make-bindings) (car opt))))
        (and (%match language input debug-info bindings)
             bindings)))

    (define (ref-bindings bindings var)
      (cond
        ((assq var (bindings->matched bindings))
         => cdr)
        (else (error "Unused <variable>."))))

    (define (construct language bindings)
      (cond
        ((variable? language) (ref-bindings bindings language))
        ((pair? language)
         (cons (construct (car language) bindings)
               (construct (cdr language) bindings)))
        (else language)))))
