; lat?: list -> bool
(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

; member?: symbol lat -> bool
(define (member? a lat)
  (cond
    ((null? lat) #t)
    ((eq? (car lat) a) #t)
    (else (member? a (cdr lat)))))
