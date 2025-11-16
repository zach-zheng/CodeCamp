; 偏函数是：
; 在部分输入上有定义
; 在部分输入上未定义或可能不终止
; 与全函数（在所有输入上都有定义）相对


(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat))) ;初始调用：从列表的第一个元素开始

;条件判断：
;如果当前元素是数字：根据该数字作为索引，在列表中选取对应位置的元素，继续查找
;如果当前元素不是数字：检查是否等于目标值 a
(define keep-looking 
  (lambda (a sorn lat)  ; symbol or number --> sorn
    (cond 
      ((number? sorn) (keep-looking a (pick sorn lat) lat)) ;接受三个参数：a（要查找的值）、sorn（当前查看的元素）、lat（原始列表）
      (else (eq? sorn a)))))

(define shift
    (lambda (pair)
        (build (first (first pair))
            (build (second (first pair))
                (second pair)))))

;pair or atom --> pora
(define align
    (lambda (pora)
        (cond 
            ((atom? pora) pora)
            ((a-pair? (first pora)) (align (shift pora)))
            (else (build (first pora) (align (second pora)))))))

(define weight*
    (lambda (pora)
        (cond 
            ((atom? pora) 1)
            (else 
              (+ (* (weight* (first pora)) 2)
                (weight* (second pora)))))))

(define shuffle
    (lambda (pora)
        (cond 
            ((atom? pora) pora)
            ((a-pair? (first pora)) (shuffle (revpair pora)))
            ((else (build (first pora) (shuffle (second pora))))))))

;Collatz 猜想（3n+1问题）函数
(define C
    (lambda (n)
        (cond
            ((one? n) 1)
            (else (cond 
                ((even? n) (C (/ n 2)))
                (else (C (add1 (* 3 n)))))))))

(define A 
    (lambda (n m)
      (cond 
        ((zero? n) (add1 m))
        ((zero? m) (A (sub1 n) 1))
        (else (A (sub1 n) (A n (sub1 m)))))))

    