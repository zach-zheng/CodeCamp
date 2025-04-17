;rember* remove-member stars
; (define (add1 x)
;   (+ x 1))

(define (rember* a l)
  (cond
    ((null? l) '())
    ((atom? (car l))
      (cond
        ((eq? (car l) a) (rember* a (cdr l)))
        (else (cons (car l) (rember* a (cdr l))))))
    (else (cons (rember* a (car l)) (rember* a (cdr l))))))


(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (cond
        ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
        (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))
;counting a 
(define (occur* a l)
  (cond
    ((null? l) 0)
    ((atom? (car l)) (cond
      ((eq? (car l) a) (add1 (occur* a (cdr l))))
      (else (occur* a (cdr l)))))
    (else (+ (occur* a (car l)) (occur* a (cdr l))))))

;subst* : substitution all of occur
(define (subst* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (cond
      ((eq? (car l) old) (cons new (subst* new old (cdr l))))
      (else (cons (car l) (subst* new old (cdr l))))))
    (else (cons (subst* new old (car l)) (subst* new old (cdr l))))))

; insertL* 
(define (insertL* new old l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (cond
      (eq? (car l) old) (cons new (cons old) (insertL* new old (cdr l)))
      (else (cons (car l) (insertL* new old (cdr l))))))
    (else (cons (insertL* new old (car l)) (insertL* new old (cdr l))))))

;member* 
(define (member* m l)
  (cond
    ((null? l) #f)
    ((atom? (car l)) (or (eq? (car l) m) (member* m (cdr l))))
    (else (or (member* m (car l)) (member* m (cdr l))))))

;leftmost: finds the atom in a non-empty list of S-expressions
(define (leftmost l)
  (cond
    ((atom? (car l)) (car l))
    (else (leftmost (car l)))))

(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((and (null? l1) (atom? (car l2))) #f)
    ((null? l1) #f)
    ((and (atom? (car l1)) (atom? (car l2))) )))