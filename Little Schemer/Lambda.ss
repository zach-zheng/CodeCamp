
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
    (else (cons (car l) (rember-f test? a (cdr l))))))

; insertL
; insertL: symbol symbol lat -> lat
; (define insertL new old lat
;     (cond
;       ((null? lat) '())
;       ((eq? old (car lat)) (cons new lat))
;       (else (cons (car lat) (insertL new old (cdr lat))))))

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

  
(define (multirememberT test? lat)
  (cond
    ((null? lat) '())
    ((test? (car lat)) (multirememberT test? (cdr lat)))
    (else (cons (car lat) (multirememberT test? (cdr lat))))))

; a is tuna , lat is (tuna), col is a-friend;
(define (multirememberco a lat col)
  (cond
    ((null? lat) (col '() '()))
    ((eq? (car lat) a) 
      (multirememberco a (cdr lat ) (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
    (else (multirememberco a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen))))))


(define (a-friend x y)
  (null? y))

; (car lat) is tuna, col is a-friend
(define (new-friend newlat seen)
  (col newlat (cons (car lat) seen)))   

(define (new-friend newlat seen)
  (a-friend newlat (cons 'tuna seen)))


;将new插入到oldL的左边,oldR的右边.
(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? oldL (car lat))
      (cons new
            (cons oldL
                  (multiinsertLR new
                                 oldL
                                 oldR
                                 (cdr lat)))))
     ((eq? oldR (car lat))
      (cons oldR
            (cons new
                  (multiinsertLR new
                                 oldL
                                 oldR
                                 (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertLR new
                           oldL
                           oldR
                           (cdr lat)))))))


(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col '() 0 0))
     ((eq? oldL (car lat))
      (multiinsertLR&co new
                        oldL
                        oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new (cons oldL newlat))
                               (add1 L)
                               R))))
     ((eq? oldR (car lat))
      (multiinsertLR&co new
                        oldL
                        oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR (cons new newlat))
                               L
                               (add1 R)))))
     (else
      (multiinsertLR&co new
                        oldL
                        oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (car lat)
                               L
                               R)))))))

(define (even? n)
  (= (* (/ n 2) 2) n))

(define (evens-only* l)
  (cond
    ((null? l) '())
    ((atom? (car l)) (cond
      ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
      (else (evens-only*  (cdr l)))))
    (else (cons (evens-only* (car l)) (evens-only* (cdr l))))))

(define (evens-only*&co l col)
  (cond
    ((null? l) (col '() 1 0))
    ((atom? (car l)) (cond
      ((even? (car l)) (evens-only*&co (cdr l) (lambda (newl p s) (col (cons (car l) newl) (* (car l) p) s))))
      (else (evens-only*&co (cdr l) (lambda (newl p s) (col newl p (+ (car l) s)))))))
    (else (evens-only*&co (car l) (lambda (al ap as) (evens-only*&co (cdr l) (lambda (dl dp ds) (col (cons al dl) (* ap dp ) (+ as ds)))))))))