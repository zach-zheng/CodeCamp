
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

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))


(define insert-g
  (lambda (seq)
    (lambda (new old l) (cond
      ((null? l) '())
      ((eq? (car l) old) (seq new old (cdr l)))
      (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL
  (insert-g (lambda (new old l) (cons new (cons old l)))))

(define insertR
  (insert-g (lambda (new old l) (cons old (cons new l)))))

(define insert-g
  (lambda (seq)
    (lambda (new old l) (cond
      ((null? l) '())
      ((eq? (car l) old) (seq new old (cdr l)))
      (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define (subst new old l)
  (cond
    ((null? l) '())
    ((eq? (car l) old) (cons new (cdr l)))
    (else (cons (car l) (subst new old (cdr l))))))

(define (seqS new old l)
  (cons new l))

(define subst 
  (insert-g seqS))


(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car aexp))

(define (value nexp)
  (cond
    ((atom? nexp) (number? nexp))
    ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
    ((eq? (operator nexp) '*) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
    (else (^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))

(define (atom-to-function x)
  (cond
    ((eq? x '+) +)
    ((eq? x '*) *)
    (else ^)))

(define (value nexp)
  (cond
    ((atom? nexp) nexp)
    (else ((atom-to-function (operator nexp)) (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))