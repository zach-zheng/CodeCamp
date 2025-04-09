(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

; +; number number -> number
(define +
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (add1 (+ a (sub1 b)))))))

; -: number number -> number
(define -
  (lambda (a b)
    (cond
     ((zero? b) a)
     (else (sub1 (- a (sub1 b)))))))

; addtup: tup -> number
(define (addtup tup)
  (cond
    ((null? tup) 0)
    (else (+ (car tup) (addtup (cdr tup))))))

; x function:
(define (* a b)
  (cond
    ((zero? b) 0)
    (else (+ a (* a (sub1 b))))))

; tup
(define (tup tup1 tup2)
  (cond
    ((and (null? tup1) (null? tup2)) '())
    ((or (null? tup1)) tup2)
    (else (cons (+ (car tup1) (car tup2))(tup (cdr tup1) (cdr tup2))))))