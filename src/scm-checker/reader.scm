(define-library (scm-checker reader)
  (import (scheme base)
          (scheme write)
          (scheme file)
          (scheme process-context)
          ;(only (scheme list) fold last)
          (only (srfi 1) fold)
          (prefix (scheme-reader core) srdr/))
  (export read-super position->pair position-children position->filename read-list1
          initial-position
          position?
          <no-position>)
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

    (define (position-newline pos)
      (make-position (position->filename pos)
                     (+ (ref-line pos) 1)
                     1))

    (define (position-inc-col pos)
      (position-append-col pos 1))

    (define no-position-children (cons #f #f))
    (define <no-position> (make-position* "NONE" 0 0 no-position-children))
    (set-car! no-position-children <no-position>)
    (set-cdr! no-position-children no-position-children)

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

      (define (%handle-list ls position use-inc?)
        (let-list (((reversed-ls pos debug-info)
                    (%handle-list* ls
                                   (if use-inc? (position-inc-col position) position))))
           (list (reverse reversed-ls)
                 (if use-inc? (position-inc-col pos) pos)
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
                  (make-position*
                    (position->filename position)
                    (ref-line position)
                    (+ (ref-col position) 1)
                    (cons (list-ref first 2)
                          (list-ref first 2))))
            tail)))

      (define (%string-move position s)
        (let loop ((i 0)
                   (line (ref-line position))
                   (col (ref-col position)))
          (cond
            ((= i (string-length s))
             (make-position* (position->filename position)
                             line
                             col
                             #f))
            ((char=? (string-ref s i) #\newline)
             (loop (+ i 1) (+ line 1) 0))
            ((and (char=? (string-ref s i) #\return)
                  (char=? (string-ref s (+ i 1)) #\newline))
             (loop (+ i 2) (+ line 1) 0))
            ((char=? (string-ref s i) #\return)
             (loop (+ i 1) (+ line 1) 0))
            (else
              (loop (+ i 1) line (+ col 1))))))

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
           (let ((l (%handle-list (srdr/lexical-data icode) (position-append-col position 3) #t)))
             (list (caar l)
                   (cadr l)
                   (list-ref l 2))))

          ((and (srdr/lexical? icode) (eq? (srdr/lexical-type icode) 'COMMENT))
            (position-newline position))

          ((and (srdr/lexical? icode) (eq? (srdr/lexical-type icode) 'ATOM))
           (list (srdr/lexical-data icode)
                 (position-append-col position (string-length (srdr/lexical-origin icode)))
                 position))
          ((and (srdr/lexical? icode) (eq? (srdr/lexical-type icode) 'STRING))
           (list (srdr/lexical-data icode)
                 (%string-move position (srdr/lexical-origin icode))
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
           (%handle-list icode position #t))
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

      (define (read-list1 port position)
        (let ((code (srdr/read-internal port)))
          (%handle-list code position #f)))

      (define (handle-shebang port position)
        (let ((code (srdr/read-internal-or-handle-shebang port)))
          (if (and (srdr/lexical? code)
                   (eq? (srdr/lexical-type code) 'SHEBANG))
            (begin
              (read-char port)
              (list '() (position-newline position) '()))
            (let-list (((constructed position debug-info)
                        (%handle-list code position #f)))
              (list (list constructed) position (list debug-info))))))

      (define (read-super filename)
        (call-with-input-file
          filename
          (lambda (port)
            (let-list (( (iconstructed-list
                          iposition
                          idebug-info-list)
                        (handle-shebang port (initial-position filename))))

              (let loop ((constructed-list iconstructed-list)
                         (position iposition)
                         (debug-info-list idebug-info-list))
                (let-list (( (constructed new-position debug-info)
                                (read-list1 port position)))
                  (if (null? constructed)
                    (values (reverse constructed-list)
                            (reverse debug-info-list))
                    (loop (cons (car constructed) constructed-list)
                          new-position
                          (cons (car (ref-children debug-info))
                                debug-info-list)))))))))))
