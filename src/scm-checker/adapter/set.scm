(define-library (scm-checker adapter set)
  ;; Adapter for Scheme implementations that do not support SRFI 113 (scheme set).
  (cond-expand
    ((or srfi-113 chicken sagittarius)
      (import (scheme base)
              (prefix (srfi 113) set/)
              (prefix (srfi 128) comparator/))
      (export make-set-eq
              list->eq-set
              eq-set->list
              ;(rename set/set-intersection intersection)
              ;(rename set/set-empty? empty?)
              ;(rename set/set-union union)
              ;(rename set/set-contains? contains?)
              intersection empty? contains? union)
      (begin
        (define intersection set/set-intersection)
        (define union set/set-union)
        (define contains? set/set-contains?)
        (define empty? set/set-empty?)

        (define eq-comparator (comparator/make-eq-comparator))
        (define (list->eq-set ls)
          (set/list->set eq-comparator ls))

        (define (eq-set->list set)
          (set/set->list set))


        (define (make-set-eq . args)
          (apply set/set
                 eq-comparator
                 args))))
    ((or srfi-1 guile kawa)
     ;; Guile Scheme and Kawa has no srfi-1 in (feature) symbols.
     (import (scheme base)
             (srfi 1))

     (export make-set-eq intersection union
             contains? empty?
             list->eq-set
             eq-set->list)
     (begin
       (define (make-set-eq . args)
         (delete-duplicates  args eq?))

       (define (eq-set->list x) x)

       (define (list->eq-set ls)
         (delete-duplicates ls eq?))

       (define empty? null?)

       (define (intersection . args)
         (apply lset-intersection eq? args))

       (define (union . args)
         (apply lset-union eq? args))

       (define (contains? st elem)
         (and (memq elem st) #t))))))
