(define-library (scm-checker match core)
  (import (scheme base)
          (only (srfi 1) fold)
          (prefix (scm-checker reader)
                  schk-rdr/)
          (prefix (scm-checker adapter box) box/))
  (export make-variable match make-bindings construct
          ref-bindings
          var1 var2 var3 var4)
  (begin
    ;;TODO: Support ellipsis pattern.

    (define-record-type <variable>
      (%make-variable name)
      variable?
      (name variable->name))

    (define *cnt* 0)
    (define (make-variable)
      (set! *cnt* (+ *cnt* 1))

      (%make-variable (string->symbol
                        (string-append "v" (number->string *cnt*)))))

    (define var1 (make-variable))
    (define var2 (make-variable))
    (define var3 (make-variable))
    (define var4 (make-variable))

    (define (make-bindings)
      (box/box '()))

    (define (check-and-bind var val bindings)
      (let ((exists-cell (assq var (box/unbox bindings))))
        (if (not exists-cell)
          (begin (box/set-box! bindings
                               (cons (cons var val) (box/unbox bindings)))
                 #t)
          (equal? val (cdr exists-cell)))))

    (define (%match language input debug-info bindings)
      (cond
        ((variable? language)
         (check-and-bind language input bindings))
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
                           (cdr (schk-rdr/position-children debug-info))
                      bindings))))
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
        ((assq var (box/unbox bindings))
         => cdr)
        (else (error "Unused <variable>."))))

    (define (construct language bindings)
      (cond
        ((variable? language) (ref-bindings bindings language))
        ((pair? language)
         (cons (construct (car language) bindings)
               (construct (cdr language) bindings)))
        (else language)))))
