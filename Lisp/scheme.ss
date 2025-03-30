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

    ;(1 10)
    ;(A 0 (A 1 9)) = (1 10)
    ;(A 0 (A 1 8)) = (A 0 (A 1 9))
    ;(A 0 (A 1 7)) = (A 0 (A 1 8))
    ;(A 0 (A 1 2)) = (A 0 (A 1 1)) = 4
    ;(A 0 (A 1 1)) = 2
    ;(2 4) 
    ;(A 1 (A 2 3)) = 
    ;(A 1 (A 2 2)) = 16 
    ;(A 1 (A 2 1)) = (A 1 2) = 4
    ;(A 2 1) = 2