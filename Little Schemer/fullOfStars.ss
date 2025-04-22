;rember* remove-member stars
; (define (add1 x)
;   (+ x 1))
; eqan: 
(define (eqan? a1 a2)
  (cond
    ((and (number? a1) (number? a2)) (= a1 a2))
    ((or (number? a1) (number? a2)) #f)
    (else (eq? a1 a2))))

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

; How many questions will eqlist?" Each argument may be either
; -empty,
; - an atom consed onto a list, or
; - a list consed onto a list.
; For example, at the same time as the first argument may be the empty list, the second argument could be the empty list or have an atom or a list in the car position."
(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((and (null? l1) (atom? (car l2))) #f)
    ((null? l1) #f)
    ((and (atom? (car l1)) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2))) (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    ((atom? (car l1)) #f)
    ((null? l2) #f)
    ((atom? (car l2)) #f)
    (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))
; eqlist? 优化
(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ;((and (null? l1) (atom? (car l2))) #f)  ;((and (atom? (car l1)) (null? l2)) #f) 
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2))) 
      (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    ((or (atom? (car l1)) (atom? (car l2))) #f)
    (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))


;equal? : S-expression is either an atom or a (empty list) list of S-expression, 2 S-expression are the same
(define (equal? s1 s2)
  (cond
    ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist? s1 s2))))

; eqlist? using equal? function
(define (eqlist? l1 l2)
  (cond
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))))