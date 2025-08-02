(define-library (scm-checker code-warning)
  (import (scheme base))
  (export make-code-warning code-warning->pos code-warning->message code-warning->suggestion
          code-warning->code
          make-code-warning-with-suggestion )
  (begin
    (define-record-type <code-warning>
      (make-code-warning* pos message code suggestion)
      code-warning?
      (pos code-warning->pos)
      (message code-warning->message)
      (code code-warning->code)
      (suggestion code-warning->suggestion))

    (define (make-code-warning pos message)
       (make-code-warning* pos message #f #f))

    (define (make-code-warning-with-suggestion pos message code suggestion)
       (make-code-warning* pos message code suggestion))))
