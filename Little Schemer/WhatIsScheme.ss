(define (member? a lat)
    (cond
        ((null? lat) #f)
        ((eq? a (car lat)) #t)
        (else (member? a (cdr lat)))))

(define (rember a lat)
    (cond
        ((null? null) '())
        ((eq? a (car lat)) (cdr lat))
        (else (cons (car lat) (rember a (cdr lat))))))

; firsts: list -> list
(define (firsts l)
    (cond ((null? l) '())
        (else (cons (car (car l)) (firsts (cdr l))))))

; insertR: symbol symbol lat -> lat
(define (insertR new old lat)
    (cond
        ((null? lat) '())
        ((eq? old (car lat)) (cons old (cons new (cdr lat))))
        (else (cons (car lat) (insertR new old (cdr lat))))))

(define (insertL new old lat)
    (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new (cons old (cdr lat))))
        (else (cons (car lat) (insertL new old (cdr lat))))))

(define (subst new old lat)
    (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new (cdr lat)))
        (else (cons (car lat) (subst new old (cdr lat))))))

(define (subst2 new old1 old2 lat)
    (cond ((null? lat) '())
        ((eq? old1 (car lat)) (cons new (cdr lat)))
        ((eq? old2 (car lat)) (cons new (cdr lat)))
        (else (cons (car lat) (subst2 new old1 old2 (cdr lat))))))

(define (subst2 new old1 old2 lat)
    (cond ((null? lat) '())
        ((or (eq? old2 (car lat))
                (eq? old1 (car lat))) (cons new (cdr lat)))
        (else (cons (car lat) (subst2 new old1 old2 (cdr lat))))))

(define (nultirember a lat)
    (cond ((null? lat) '())
        ((eq? a (car lat)) (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat))))))

(define (multiinsertR new old lat)
    (cond ((null? lat) '())
        ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat) (multiinsertR new old (cdr lat))))))

(define (multiinsertL new old lat)
    (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat))))))

(define (multisubst new old lat)
    (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
        (else (cons (car lat) (multisubst new old (cdr lat))))))

(define add1 1+)
(define sub1 1-)

(define (+ a b)
    (cond ((zero? b) a)
        (else (+ (add1 a) (sub1 b)))))

(define (- a b)
    (cond ((zero? b) a)
        (else (- (sub1 a) (sub1 b)))))

(define (addtup tup)
    (cond ((null? tup) 0)
        (else (+ (car tup) (addtup (cdr tup))))))

(define (* n m)
    (cond ((zero? m) 0)
        (else (+ n (* n (sub1 m))))))

(define (tup1+ tup1 tup2)
    (cond ((null? tup2) tup1)
        ((null? tup1) tup2)
        (else (cons (+ (car tup1) (car tup2))
                    (tup1+ (cdr tup1) (cdr tup2))))))

(define (> n m)
    (cond ((zero? n) #f)
        ((zero? m) #t)
        (else (> (sub1 n) (sub1 m)))))
(define (< n m)
    (cond ((zero? m) #f)
        ((zero? n) #t)
        (else (< (sub1 n) (sub1 m)))))

(define (= n m)
    (cond ((zero? m) (zero? n))
        ((zero? n) #f)
        (else (= (sub1 n) (sub1 m)))))

(define (^ n m)
    (cond 
        ((zero? n) 0)
        ((zero? m) 1)
        (else (* n (^ n (sub1 m))))))

(define (/ n m)
    (cond 
        ((< n m) 0)
        (else (add1 (/ (- n m) m)))))

(define (length lat)
    (cond ((null? lat) 0)
        (else (add1 (length (cdr lat))))))

(define (pick n lat)
    (cond ((zero? (sub1 n)) (car lat))
        (else (pick (sub1 n) (cdr lat)))))

(define (rempick n lat)
    (cond ((null? lat) '())
        ((zero? (sub1 n)) (cdr lat))
        (else (cons (car lat) (rempick (sub1 n) (cdr lat))))))

(define (no-nums lat)
    (cond ((null? lat) '())
        ((number? (car lat)) (no-nums (cdr lat)))
        (else (cons (car lat) (no-nums (cdr lat))))))

(define (all-nums lat)
    (cond ((null? lat) '())
        ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
        (else (cons (all-nums (cdr lat))))))

(define (eqan? a1 a2)
    (cond ((and (number? a1) (number? a2)) (= a1 a2))
        ((or (number? a1) (number? a2)) #f)
        (else (eq? a1 a2))))

(define (occur a lat)
    (cond ((null? lat) 0)
        ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
        (else (occur a (cdr lat)))))

(define (one? n)
    (cond ((zero? n) #f)
        (else (zero? (sub1 n))))) 