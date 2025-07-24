(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (only (scheme list) fold append-map)
        (prefix (scm-check reader) schk-rdr/)
        )

(define-record-type <code-warning>
  (make-code-warning pos message)
  code-warning?
  (pos ref-pos)
  (message ref-message))

(define (check-import* code debug-info)
  (fold
    (lambda (code* debug-info* accum)
      (let* ((library-name code*)
             (appended-import
              (cons code* (car accum))))
        (if (member library-name (car accum))
          (list appended-import
                (cons (make-code-warning debug-info*
                                         "Duplicate import.")
                      (cadr accum)))
          (list appended-import
                (cadr accum)))))
    (list '();;used librarynames
          '());;error
    (cdr code)
    (cdr (schk-rdr/position-children debug-info))))


(define (check-import code debug-info)
  (cadr (check-import* code debug-info)))

(define (check-code code debug-info)
  (cond
    ((not (list? code)) '())
    ((eq? (car code) 'import)
     (check-import code debug-info) )
    (else '())))

(define (check-file filename)
  (let-values (((code debug-info) (schk-rdr/read-super filename)))
    (append-map
      check-code
      code
      debug-info)))

(define (print-warn warn)
  (let ((pos-pair (schk-rdr/position->pair (ref-pos warn))))
    (display "Warning:")
    (display "L")
    (display (car pos-pair))
    (display "C")
    (display (cdr pos-pair))
    (display " ")
    (display (ref-message warn))
    (newline)))

(define (main)
  (let ((args (command-line)))
    (when (>= (length args) 2)
      (for-each
        print-warn
        (check-file (cadr args))))))

(main)
