(cond-expand
  (chicken
   (include "scheme-reader/scheme-reader/core.scm")
   (include "scm-check/reader.scm")
   (include "scm-check/code-warning.scm")
   (include "scm-check/check-component/import.scm")
   (include "scm-check/check-component/cond.scm"))
  (else))

(import (scheme base)
        (scheme write)
        (scheme file)
        (scheme process-context)
        (only (srfi 1) append-map)
        (prefix (scm-check reader) schk-rdr/)
        (prefix (scm-check code-warning) w/)
        (prefix (scm-check check-component import) chk-import/)
        (prefix (scm-check check-component cond) chk-cond/))

(define (check-code code debug-info)
  (cond
    ((not (list? code)) '())
    ((eq? (car code) 'import)
     (chk-import/check-import code debug-info) )
    ((eq? (car code) 'cond)
     (chk-cond/check-cond code debug-info) )
    (else '())))

(define (check-file filename)
  (let-values (((code debug-info) (schk-rdr/read-super filename)))
    (append-map
      check-code
      code
      debug-info)))

(define (print-warn warn)
  (let ((pos-pair (schk-rdr/position->pair (w/code-warning->pos warn)))
        (filename (schk-rdr/position->filename (w/code-warning->pos warn))))
    (display filename)
    (display ":")
    (display (car pos-pair))
    (display ":")
    (display (cdr pos-pair))
    (display ":")
    (display "W")
    (display ":")
    (display (w/code-warning->message warn))
    (newline)))

(define (main)
  (let ((args (command-line)))
    (when (>= (length args) 2)
      (for-each
        print-warn
        (check-file (cadr args))))))

(main)
