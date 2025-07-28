(cond-expand
  (chicken
   (include "scheme-reader/scheme-reader/core.scm")
   (include "scm-check/reader.scm")
   (include "scm-check/code-warning.scm")
   (include "scm-check/check-component/import.scm")
   (include "scm-check/check-component/if.scm")
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
        (prefix (scm-check check-component if) chk-if/)
        (prefix (scm-check check-component cond) chk-cond/))

(define (handle-list code debug-info)
  (let ((debug-info-list* (schk-rdr/position-children debug-info)))
    (append-map
      (lambda (code* debug-info*) (check-code code* debug-info*))
        code
        debug-info-list*)))

(define (check-code code debug-info)
  (cond
    ((not (list? code)) '())
    ((null? code) '())
    ((eq? (car code) 'import)
     (chk-import/check-import code debug-info) )
    ((eq? (car code) 'cond)
     (append (chk-cond/check-cond code debug-info)
             (handle-list code debug-info)))
    ((eq? (car code) 'if)
     (append (chk-if/check-if code debug-info)
             (handle-list code debug-info)))
    ((eq? (car code) 'quote)
      '())
    ((list? code)
     (handle-list code debug-info))
    (else '())))

(define (check-file filename)
  (let-values (((code debug-info) (schk-rdr/read-super filename)))
    (append-map
      check-code
      code
      debug-info)))

(define (check-stdin)
  (let loop ((position (schk-rdr/initial-position "-")))
    (let ((constructed-list (schk-rdr/read-list1 (current-input-port) position)))
      (unless (null? (car constructed-list))
        (for-each
          print-warn
          (check-code (caar constructed-list)
                      (car (schk-rdr/position-children (list-ref constructed-list 2)))))
        (loop (cadr constructed-list))))))

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
    (when (= (length args) 2)
      (cond
        ((string=? (cadr args) "-")
         (check-stdin))
        (else
          (for-each
            print-warn
            (check-file (cadr args))))))))
(main)
