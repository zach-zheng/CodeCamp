; 三个数的参数，返回其中较大的两个数之和。
; (if <predicate> <consequent> <alternative>)
(define (sumGreater x y z)
    (if (>= x y)
      (if (> y z)
        (+ x y)
        (+ x z))
      (if (>= x z)
        (+ y x)
        (+ y z))))

; 练习1.3 定义一个过程，三个数的参数，返回其中较大的两个数平方和。
; cond  (<p1> <e1>)
;       (<p2> <e2>)
;       (<pn> <en>)
;       (else <en>)

(define (sum-squares x y z)
  (cond
    ((and (>= x y) (>= y z)) (+ (* x x) (* y y)))
    ((and (>= y x) (>= z x)) (+ (* y y) (* z z)))
    (else (+ (* x x) (* z z)))))

; 练习 1.4 函数作为值转递给参数
(define (a-plus-absB a b)
  ((if (> b 0)
    +
    -) a b))


;X平方根 = y, and y >= 0, y * y =x
; y 和 x/y 的平均值
; (y + x/y) / 2

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (squares guess) x)) 0.001))
    (define (improve guess)
      (average guess (/ x guess)))
      (define (sqrt-iter guess)
        (if (good-enough? guess)
          guess
          (sqrt-iter (improve guess))))
          (sqrt-iter 1.0))

(define (A x y)
  (cond
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1)(A x (- y 1))))))

; rember: symbol lat -> lat
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

; multirember: symbol lat -> lat
(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      (else 
        (cond
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))

; subst(substitution): symbol symbol lat -> lat
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

    
; subst2
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      (else (cond
        ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
        (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))))

; multisubst
(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

; insertR: symbol symbol lat -> lat
(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))


; multiinsertR: symbol symbol lat -> lat
(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

; insertL: symbol symbol lat -> lat
(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

; multiinsertL 
(define multiinsertL 
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      (else
        (cond
        ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
        (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))