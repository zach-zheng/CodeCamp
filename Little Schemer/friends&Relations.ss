;about set: no atom appears more than once.

; member?: symbol lat -> bool
(define (member? a lat)
  (cond
    ((null? lat) #t)
    ((eq? (car lat) a) #t)
    (else (member? a (cdr lat)))))

;set: no atom appears more than once.
(define (set? lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat)))))

;makeset: 
(define (makeset lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
    (else (cons (car lat) (makeset (cdr lat))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else 
        (cond
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))

;Try to write makeset using multirember
(define (makeset lat)
  (cond
    ((null? lat) '())
    (else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))

;subset:
(define (subset? set1 set2)
  (cond
    ((null? set1) #t)
    ((member? (car set1) set2) (subset? (cdr set1) set2))
    (else #f)))

;eqset:
(define (eqset? set1 set2)
    (and (subset? set1 set2) (subset? set2 set1)))

;insertset:
(define (insertsect? set1 set2)
  (cond
    ((null? set1) #f)
    ((member? (car set1) set2) #t)
    (else (subset? (cdr set1) set2))))

(define (insertsect? set1 set2)
  (cond
    ((null? set1) #f)
    (else (or (member? (car set1) set2) (subset? (cdr set1) set2)))))

(define (insertsect? set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (insertsect (cdr set1) set2)))
    (else (insertsect? (cdr set1) set2))))

;union
(define (union set1 set2)
  (cond
    ((null? set1) '())
    ()))