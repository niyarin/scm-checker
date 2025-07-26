(import (scheme write)
        (scheme base)
        (only (scheme base) cons)
        (scheme read))

(cond
  ((eq? x 'aaa) #t)
  ((eq? x 'bbb) #f)
  (else #f))

