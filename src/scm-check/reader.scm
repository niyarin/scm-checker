(define-library (scm-check reader)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)
          ;(only (scheme list) fold last)
          (only (srfi 1) fold last)
          (prefix (scheme-reader core) srdr/))
  (export read-super position->pair position-children position->filename)
  (begin
    (define-record-type <position>
      (make-position* filename line col children)
      position?
      (filename position->filename)
      (line ref-line)
      (col ref-col)
      (children ref-children))

    (define position-children ref-children)

    (define (make-position filename line col)
      (make-position* filename line col #f))

    (define (position->pair position)
      (cons (ref-line position)
            (ref-col position)))

    (define (position-append-col pos col-diff)
      (make-position (position->filename pos)
                     (ref-line pos)
                     (+ (ref-col pos) col-diff)))

    (define (position-inc-col pos)
      (position-append-col pos 1))

    (define (initial-position filename) (make-position* filename 1 1 '()))

    (define-syntax let-list
      (syntax-rules ()
        ((_ (( vars ls)) bodies ...)
         (apply (lambda vars bodies ...) ls))))

      (define (%handle-list* ls position)
        (fold
          (lambda (x accum)
            (let-list (((sub-code pos debug-info) accum))
              (let ((constructed (construct-code x pos)))
                (if (list? constructed)
                  (let-list ((( x* pos* debug-info*) constructed))
                    (list (cons x* sub-code)
                          pos*
                          (cons debug-info* debug-info)))
                  (list sub-code constructed debug-info)))))
          (list '() position '())
          ls))

      (define (%handle-list ls position)
        (let-list (((reversed-ls pos debug-info)
                    (%handle-list* ls
                                   (position-inc-col position))))
           (list (reverse reversed-ls)
                 (position-inc-col pos)
                 (make-position*
                   (position->filename position)
                   (ref-line position)
                   (+ (ref-col position) 1)
                   (reverse debug-info)))))

      (define (%handle-pair pair position)
        (let* ((first (construct-code (car pair) (position-inc-col position)))
               (next-pos (if (list? first) (cadr first) first))
               (tail (construct-code (cdr pair) next-pos)))
          (if (list? first)
            (list (cons (car first)
                        (car tail))
                  (position-inc-col (cadr tail))
                  position)
            tail)))

      (define (construct-code icode position)
        ;; return (constructed object, new-position, current position)
        ;;        IF icode is code element ELSE new-position
        (cond
          ((and (srdr/lexical? icode) (eq? (srdr/lexical-type icode) 'SPACE))
           (position-append-col position 1))
          ((and (srdr/lexical? icode) (eq? (srdr/lexical-type icode) 'NEWLINE))
            (make-position (position->filename position)
                           (+ (ref-line position) 1)
                           0))

          ((and (srdr/lexical? icode) (eq? (srdr/lexical-type icode) 'DOT-PAIR))
           (let ((l (%handle-list (srdr/lexical-data icode) (position-append-col position 3))))
             (list (caar l)
                   (cadr l)
                   (list-ref l 2))))

          ((and (srdr/lexical? icode) (eq? (srdr/lexical-type icode) 'COMMENT))
            (position-append-col position
                                 (+ 1 (string-length (srdr/lexical-data icode)))))

          ((and (srdr/lexical? icode) (eq? (srdr/lexical-type icode) 'ATOM))
           (list (srdr/lexical-origin icode)
                 (position-append-col position (string-length (srdr/lexical-origin icode)))
                 position))

          ;((and (srdr/lexical? icode))
          ; (display (srdr/lexical-type icode))(newline)
          ; (write (srdr/lexical-origin icode))(newline))

          ((symbol? icode)
           (list icode
                 (position-append-col position (string-length (symbol->string icode)))
                 position))
          ((char? icode)
           ;;;#\newline とかも平気?
            (list icode
                 (position-append-col position 3)
                 position))
          ((list? icode)
           (%handle-list icode position))
          ((pair? icode) (%handle-pair icode position))

          ((number? icode)
           (list icode
                 (position-append-col position (string-length (number->string icode)))
                 position))
          ((string? icode)
           (list icode
                 (position-append-col position (+ (string-length icode) 2))
                 position))

          ((eof-object? icode)
           position)
          (else
            (display "ELSE!")
            (write icode)
            (newline))))

      (define (read-super filename)
        (call-with-input-file
          filename
          (lambda (port)
            (let ((position (initial-position filename)))
              (let loop ((constructed-list '())
                         (position position)
                         (debug-info-list '()))
                (let ((code (srdr/read-internal port)))
                  (let-list (( (constructed new-position debug-info) (%handle-list code position)))
                    (if (null? constructed)
                      (values (reverse constructed-list)
                              (reverse debug-info-list))
                      (loop (cons (car constructed) constructed-list)
                            new-position
                            (cons (car (ref-children debug-info))
                                  debug-info-list))))))))))))
