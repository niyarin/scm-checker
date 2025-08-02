(define-library (scm-checker adapter box)
  (cond-expand
    (srfi-111
      (import (scheme base) (srfi 111))
      (export box unbox box? set-box!))
    (else
      (import (scheme base))
      (export box unbox box? set-box!)
      (begin
        (define-record-type <box>
          (box v)
          box?
          (v unbox set-box!))))))

