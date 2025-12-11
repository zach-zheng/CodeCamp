;递归计算过程
(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

;迭代计算过程
(define (factorial n)
    (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
    (if (> count max-count)
        product
        (fact-iter (* counter product)
                    (+ counter 1)
                    max-count)))
;递归计算过程
(define (+ a b)
    (if (= a 0)
        b
        (inc (+ (dec a) b))))       
;迭代计算过程
(define (+ a b)
    (if (= a 0)
        b
        (+ (dec a) (inc b)))) 


           