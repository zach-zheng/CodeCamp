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