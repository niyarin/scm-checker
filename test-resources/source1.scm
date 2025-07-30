(import (scheme write)
        (scheme base)
        (only (scheme base) cons)
        (scheme read))

(and (cons 1 2)
     (and (cons 3 4) (cons 100 200))
     (cons 5 6))


(if test foo #f)

(if #t foo 3)
(if #f foo 3)

(cond
  ((zero? x) #f)
  ((= x 3) #t)
  ((eq? x (quote a)) #t)
  (else "HELLO"))

(and 100000)
