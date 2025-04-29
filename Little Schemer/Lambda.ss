
(define (rember* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
      (cond
        ((eq? (car l) a) (rember* a (cdr l)))
        (else (cons (car l) (rember* a (cdr l))))))
    (else (cons (rember* a (car l)) (rember* a (cdr l))))))


(define (rember-f test? a l)
  (cond
    ((null? l) '())
    ((test? (car l) a) (cdr l))
    (else (cons (car l) (rember-f test? a (cdr l)))))

; insertL
; insertL: symbol symbol lat -> lat
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define insertL-f (lambda (test?)
  (lambda (new old l) 
    (cond
      ((null? l) '())
      ((test? (car l) old) (cons new (cons old (cdr l)))) 
      (else (cons (car l)((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l) (cond
      ((null? l) '())
      ((test? (car l) old) (cons old (cons new (cdr l))))
      (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

