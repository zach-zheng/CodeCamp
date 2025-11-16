(define rember-f 
    (lambda (test? a l)
        (cond ((null? l) '())
            (else (cond 
                ((test? (car l) a) (cdr l))
                (else (cons (car l) (rember-f test? a (cdr l)))))))))

(define rember-f 
    (lambda (tests?)
        (lambda (a l)
            (cond ((null? l) '())
                ((test? (car l) a) (cdr l))
                (else (cons (car l) ((rember-f test?) a (cdr l))))))))


(define insertR-f
    (lambda (test?)
        (lambda (new old lat)
         (cond
         ((null? lat) '())
         ((test? old (car lat)) (cons old (cons new (cdr lat))))
         (else (cons (car lat) ((insertR-f test?) new old (cdr lat))))))))

(define (insertL new old lat)
    (cond ((null? lat) '())
        ((eq? old (car lat)) (cons new (cons old (cdr lat))))
        (else (cons (car lat) (insertL new old (cdr lat))))))



(define seqL
    (lambda (new old l)
     (cons new (cons old l))))
(define seqR
    (lambda (new old l)
     (cons old (cons new l))))

;通用算法
(define insert-g 
  (lambda (seq)
    (lambda (new old l) 
      (cond ((null? l) '())
        ((eq? (car l) old) 
         (seq new old (cdr l)))   ;可变部分
        (else 
         (cons (car l) ((insert-g seq) new old (cdr l))))))))
        
;具体实现
(define insertL 
    (insert-g 
     (lambda (new old l)
      (cons old (cons new l)))))
;具体实现
(define insertR 
    (insert-g 
     (lambda (new old l)
      (cons old (cons new l)))))


;; 通用模板
(define (make-insertion seq-function)
  (lambda (new old lst)     ;返回函数
    (cond ((null? lst) '())
          ((eq? (car lst) old) 
           (seq-function new old (cdr lst)))  ; 可变部分
          (else 
           (cons (car lst) 
                 ((make-insertion seq-function) new old (cdr lst)))))))

;; 具体实现
(define insertR (make-insertion 
                 (lambda (new old rest) 
                   (cons old (cons new rest)))))

;; 具体实现
(define insertL (make-insertion 
                 (lambda (new old rest) 
                   (cons new (cons old rest)))))


;通用算法
(define insert-g 
  (lambda (seq)
    (lambda (new old l) 
      (cond ((null? l) '())
        ((eq? (car l) old) 
         (seq new old (cdr l)))   ;可变部分
        (else 
         (cons (car l) ((insert-g seq) new old (cdr l))))))))

; (define subst
;     (lambda (new old l) 
;      (cond ((null? l) '())
;         ((eq? (car l) old) (cons new (cdr l)))
;         (else (cons (car l) (subst new old (cdr l)))))))
;具体实现
(define seqS
     (lambda (new old l)
      (cons new l)))
(define subst
    (insert-g seqS))

(define subst
  (insert-g 
    (lambda (new old l)
      (cons new l))))


(define rember
  (lambda (a l) 
    ((insert-g seqrem)) #f a l))

(define seqrem
  (lambda (new old l) 
    l))

;col - collect, 第一个参数是删除与a相同表达式后的新列表，第二个参数是被删除的值(a表达式)组成的列表
; CPS 实现同时返回删除后的列表和被删除的元素
(define multirember&co
  (lambda (a lat col)
    (cond 
      ((null? lat) (col '() '()))
      ((eq? a (car lat)) (multirember&co a (cdr lat) 
        (lambda (newlat seen) (col newlat (cons (car lat) seen)))))
        (else (multirember&co a (cdr lat) (lambda (newlat seen) (col (cons (car lat) newlat) seen)))))))


(define multiinsertLR&co
  (lambda (new oldL oldR lat col) 
    (cond 
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL) (multiinsertLR&co new oldL oldR (cdr lat)
        (lambda (newlat L R)
          (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR) (multiinsertLR&co new oldL oldR (cdr lat)
        (lambda (newlat L R)
          (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat) 
        (lambda (newlat L R)
          (col (cons (car lat) newlat L R))))))))


(define even? (lambda (n)
  (= (* (/ n 2) 2) n)))

(define evens-only* 
  (lambda (l)
    (cond 
      ((null? l) '())   ; empty
      ((atom? (car l))  ; an atom consed onto a list
        (cond 
        ((even? (car l)) (cons (car l) (evens-only* (cdr l))))  ; a list consed onto a list
        (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))))

(define evens-only*&co
  (lambda (l col)
  (cond 
    ((null? l) (col '() 1 0))
    ((atom? (car l)) 
      (cond 
        ((even? (car l)) (evens-only*&co (cdr l)
          (lambda (newl p s) (col (cons (car l) newl) (* (car l) p) s))))
        (else (evens-only*&co (cdr l) 
          (lambda (newl p s) (col newl p (+ (car l) s)))))))
    (else (evens-only*&co (car l)
          (lambda (al ap as) ; al -> car list, ap -> car product, as -> car sum
          (evens-only*&co (cdr l) (lambda (dl dp ds)(col (cons al dl) (* ap dp) (+ as ds))))))))))       ; dl -> cdr list, dp -> cdr product, ds -> cdr sum

(define length(lambda (l)
  (cond 
    ((null? l) 0)
    (else (add1 (length (cdr l)))))))


;length0
    (lambda (l)
      (cond 
        ((null? l) 0)
        (else (add1 (eternity (cdr l))))))

    ((lambda (length)
      (lambda (l)
        (cond 
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    eternity)

    (lambda (mk-length)
        (mk-length eternity))
      (lambda (length)
        (lambda (l) 
          (cond 
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))

    (lambda (mk-length)
        (mk-length mk-length))
      (lambda (length)
        (lambda (l)
          (cond 
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))

    (lambda (mk-length)
        (mk-length mk-length))
      (lambda (mk-length)
        (lambda (l)
          (cond 
            ((null? l) 0)
            (else (add1 (mk-length (cdr l)))))))


;length<=1
    (lambda (l)
      (cond 
        ((null? l) 0)
        (else (add1 (length0 (cdr l))))))

    ((lambda (f)
      (lambda (l)
        (cond 
          ((null? l) 0)
          (else (add1 (f (cdr l)))))))
    ((lambda (g)
      (lambda (l)
        (cond 
          ((null? l) 0)
          (else (add1 (g (cdr l)))))))
    eternity))

    (lambda (mk-length)
        (mk-length) (mk-length eternity))
      (lambda (length)
        (lambda (l) 
          (cond 
            ((null? l) 0)
            (else (add1 (length (cdr l)))))))

;length<=1
    (lambda (l)
      (cond 
        ((null? l) 0)
        (else (add1 ((lambda (l)
                      (cond 
                        ((null? l) 0)
                        (else (add1 (eternity (cdr l)))))) (cdr l))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((mk-length mk-length) (cdr l))))))))

((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

;  F = (lambda (mk-length)
;    ((lambda (length)
;       (lambda (l)
;         (cond
;          ((null? l) 0)
;          (else (add1 (length (cdr l)))))))
;     (lambda (x)
;       ((mk-length mk-length) x))))

  ((lambda (mk-length)
   (mk-length mk-length)) F)

 (lambda (x)
     ((F F) x))