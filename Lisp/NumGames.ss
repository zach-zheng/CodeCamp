
;非标准函数（如 atom?、eqan?）
(define (atom? x)
  (and (not (null? x)) (not (pair? x))))

(define (eqan? a b)
  (cond
    ((and (number? a) (number? b)) (= a b))
    ((or (number? a) (number? b)) #f)
    (else (eq? a b))))


(define (add1 x)
  (+ x 1))

(define sub1 (lambda (x) (- x 1)))

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

; multiplay function:
(define (* a b)
  (cond
    ((zero? b) 0)
    (else (+ a (* a (sub1 b))))))

; tup
(define (tup tup1 tup2)
  (cond
    ((and (null? tup1) (null? tup2)) '())
    ((null? tup1) tup2)
    ((null? tup2) tup1)
    (else (cons (+ (car tup1) (car tup2)) (tup (cdr tup1) (cdr tup2))))))

; >
(define (> n m)
  (if (zero? m)
      (not (zero? n))
      (if (zero? n)
          #f
          (> (sub1 n) (sub1 m)))))

; <
(define (< n m)
  (if (zero? n)
    (not (zero? m))
    (if (zero? m)
      #f
      (< (sub1 n) (sub1 m)))))

; =
(define =
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (= (sub1 n) (sub1 m))))))

; exponent 
(define expo
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (expo n (sub1 m)))))))

; division /
(define div
  (lambda (a b)
    (cond
      ((< a b) 0)
      (else (add1 (div (- a b) b)))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat))))))