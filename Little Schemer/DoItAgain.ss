; lat?: list -> bool
(define (lat? l)
  (cond
    ((null? l) #t)
    ((atom? (car l)) (lat? (cdr l)))
    (else #f)))

; member?: symbol lat -> bool
(define (member? a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat)))))

(define (member?&co a lat col)
  (cond
    ((null? lat) (col '() '()))
    ((eq? (car lat) a) (member?&co a (cdr lat) (lambda (in out) (col (cons a in) out))))
    (else member?&co a (cdr lat) (lambda (in out) (col in (cons a out))))))

(define (col lat nolat)
  (null? nolat))

(define (lat?&co l col)
  (cond
    ((null? l) (col '() '()))
    ((atom? (car l)) (lat?&co (cdr l) (lambda (lat nolat) (col (cons (car l) lat) nolat))))
    (else (lat?&co (cdr l) (lambda (lat nolat) (col lat (cons (car l) nolat)))))))


(define (keep-looking a sorn lat)
  (cond
    ((number? sorn) (keep-looking a (pick sorn lat) lat))
    (else (eq? sorn a))))

(define (rember&co a lat col)
  (cond
    ((null? lat) (col 0 '()))
    ((eq? a (car lat)) (col 1 (cdr lat)))
    (else (rember&co a (cdr lat) (lambda (rmcount leftlat) (col rmcount (cons (car lat) leftlat)))))))


(define (shift pair)
  (build (first (first pair))
    (build (second (first pair))
      (second pair))))

(define (shuffle pora)
  (cond
    ((atom? pora) pora)
    ((a-pair? (car pora)) (shuffle (revpair pora)))
    (else (build (car pora) (shuffle (cdr pora))))))


; (A 1 0) 2; (A 2 1)
; (A 1 1) 3   (A 0 (A (1 0))) - (A 0 (A 0 1)) - (A 0 2) 3
; (A 2 2) 7

(define (A n m)
  (cond
    ((zero? n) (add1 m))
    ((zero? m) (A (sub1 n) 1))
    (else (A (sub1 n)
          (A n (sub1 m))))))

(define (length l)
  (cond
    ((null? l) 0)
    (else (add1 (length (cdr l))))))

(lambda (l) (cond
  ((null? l) 0)
  (else (add1 (eternity (cdr l))))))

(lambda (l) (cond
  ((null? l) 0)
  (else (add1 (length (cdr l))))))

(lambda (l) (cond
  ((null? l) 0)
  (else (add1 
    ((lambda (l) 
      (cond
        ((null? l) 0)
        (else (add1 (eternity (cdr l)))))) 
    (cdr l))))))


;; length0
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (mk-length (cdr l))))))))

((lambda (mk-length) (mk-length mk-length))
  (lambda (mk-length) 
  ((lambda (length)
    (lambda (l) 
    (cond
      ((null? l) 0) 
      (else (add1 (length (cdr l)))))))
    (lambda (x)
    ((mk-length mk-length) x)))))

((lambda (le)
  ((lambda (mk-length)
    (mk-length mk-length)) (lambda (mk-length)
  (le (lambda (x)
      ((mk-length mk-length) x))))))
(lambda (length) 
  (lambda (l)
   (cond
    ((null? l) 0)
    (else (add1 (length (cdr l))))))))

(define (list-to-action e)
  (cond
    ((atom? (car e)) (cond
      ((eq? (car e) 'quote) *quote)
      ((eq? (car e) 'lambda) *lambda)
      ((eq? (car e) 'cond) *cond)
      (else *application)))
    (else *application)))

(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*lambda e table)
  (build 'non-primitive (cons table (cdr e))))