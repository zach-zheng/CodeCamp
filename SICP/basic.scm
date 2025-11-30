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
