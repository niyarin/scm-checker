(define-library (scm-check code-warning)
  (import (scheme base))
  (export make-code-warning code-warning->pos code-warning->message)
  (begin
    (define-record-type <code-warning>
      (make-code-warning pos message)
      code-warning?
      (pos code-warning->pos)
      (message code-warning->message))))

