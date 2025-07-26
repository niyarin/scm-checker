(cond-expand
  (chicken
   (include "scheme-reader/scheme-reader/core.scm")
   (include "scm-check/reader.scm"))
  (else))

(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        ;(only (scheme list) fold append-map)
        (srfi 1)
        (prefix (scm-check reader) schk-rdr/))

(define-record-type <code-warning>
  (make-code-warning pos message)
  code-warning?
  (pos ref-pos)
  (message ref-message))

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
  (let ((pos-pair (schk-rdr/position->pair (ref-pos warn)))
        (filename (schk-rdr/position->filename (ref-pos warn))))
    (display filename)
    (display ":")
    (display (car pos-pair))
    (display ":")
    (display (cdr pos-pair))
    (display ":")
    (display "W")
    (display ":")
    (display (ref-message warn))
    (newline)))

(define (main)
  (let ((args (command-line)))
    (when (>= (length args) 2)
      (for-each
        print-warn
        (check-file (cadr args))))))

(main)
