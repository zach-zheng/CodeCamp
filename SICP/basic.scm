;三个参数，返回其中较大的两个数之和
(define (two-bigger-plus a b c)
    (cond 
        ((and (> a b) (> b c)) (+ a b))
        ((and (> a b) (> c b)) (+ a c))
        (else (+ b c))))

(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b)) 

;Call by Value
;应用序导致无限循环
(define (p) (p))   
(define (test x y) 
    (if (= x 0)
        0
        y))
;(test 0 (p))

(define (sqrt x)
    (the y (and (>= y 0)
        (= (square y) x))))

;牛顿法计算平方根迭代公式：guess_new = 1/2(guess_old + x/guess_old)
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (improve guess x)
    (average guess (/ x guess)))
    
(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

;平方根计算函数的初始猜测值 1.0
(define (sqrt x)
    (sqrt-iter 1.0 x))