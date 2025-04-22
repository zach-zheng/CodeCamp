; arithmetic expression: an atom (including numbers) or two arithmetic expression combined by +, * or expo

(define (numbered? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    ((eq? (car (cdr aexp)) '+) (
      and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp)) '*) (
      and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
    ((eq? (car (cdr aexp)) 'expo) (
      and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))))

;numbered? a simpler way
(define (numbered? aexp)
  (cond
    ((atom? aexp) (number? aexp))
    (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))))

; (define (numbered? nexp)
;   (cond
;     ((atom? nexp) (number? nexp))
;     ((list? nexp) (numbered? aexp))
;     (else (and (numbered? (car nexp)) (numbered? (car (cdr (cdr nexp))))))))

(define (1st-sub-exp aexp)
  (car (cdr aexp)))

(define (2nd-sub-exp aexp)
  (car (cdr (cdr aexp))))

(define (operator aexp)
  (car aexp))

(define (value nexp)
  (cond
    ((atom? nexp) (number? nexp))
    ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
    ((eq? (operator nexp) '*) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
    (else (^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))