(define (numbered? aexp)
    (cond ((atom? aexp) (number? aexp))
        ((eq? (car (cdr aexp)) '+) 
        (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
        ((eq? (car (cdr aexp)) 'x) 
        (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
        ((eq? (car (cdr aexp)) '^) 
        (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
        ))

(define (numbered? aexp)
    (cond ((atom? aexp) (number? aexp))
        (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))))

(define (value nexp)
    (cond ((atom? nexp) nexp)
        ((eq? (car (cdr nexp)) '+) (+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
        ((eq? (car (cdr nexp)) '*) (* (value (car nexp)) (value (car (cdr (cdr nexp))))))
        (else (^ (value (car nexp)) (value (car (cdr (cdr nexp))))))))

(define (1st-sub-exp aexp)
    (car (cdr (aexp))))
(define (2nd-sub-exp aexp)
    (car (cdr (cdr (aexp)))))
(define (operator aexp)
    (car aexp))

(define (value nexp)
    (cond ((atom? nexp) nexp)
        ((eq? (operator nexp) '+) (+ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
        ((eq? (operator nexp) '*) (* (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))
        (else (^ (value (1st-sub-exp nexp)) (value (2nd-sub-exp nexp))))))

(define (sero? n)
    (null? n))

(define (edd1 n)
    (cons '() n))

(define (zub1 n)
    (cdr n))

(define (set? lat)
    (cond ((null? lat) #t)
        ((member? (car lat) (cdr lat)) #f)
        (else (set? (cdr lat)))))

(define (makeset lat)
    (cond ((null? lat) '())
        ((member? (car lat) (cdr lat)) ((makeset (cdr lat))))
        (else (cons (car lat) (makeset (cdr lat))))))

(define (makeset lat)
    (cond ((null? lat) '())
        (else (cons (car lat) (makeset (multirember (car lat) (cdr lat)))))))

(define (subset? set1 set2)
    (cond ((null? set1) #t)
        ((member? (car set1) set2) (subset? (cdr set1) set2))
        (else #f)))

(define (subset? set1 set2)
    (cond ((null? set1) #t)
        (else (and (member (car set1) set2) (subset? (cdr set1) set2)))))    

(define (eqset? set1 set2)
    (cond ((subset? set1 set2) (subset? set2 set1))
        (else #f)))    

(define (eqset? set1 set2)
     (and (subset? set1 set2) (subset? set2 set1)))

(define (intersect? set1 set2)
    (cond ((null? set1) #f)
        ((member? (car set1) set2) #t)
        (else (intersect? (cdr set1) set2))))

(define (intersect? set1 set2)
    (cond ((null? set1) #f)
        (else (or (member? (car set1) set2) (intersect? (cdr set1) set2)))))

(define (intersect set1 set2)
    (cond ((null? set1) '())
        ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
        (else (intersect (cdr set1) set2))))

(define (union set1 set2)
    (cond ((null? set1) set2)
        ((member? (car set1) set2) (union (cdr set1) set2))
        (else (cons (car set1) (union (cdr set1) set2)))))

(define (differ set1 set2)
    (cond ((null? set1) '())
        ((member? (car set1) set2) (differ (cdr set1) set2))
        (else (cons (car set1) (differ (cdr set1) set2)))))

(define (intersectall l-set)
    (cond ((null? (cdr l-set)) (car l-set))
        (else (intersect (car l-set) (intersectall (cdr l-set))))))

(define (a-pair? x)
    (cond ((atom? x) #f)
        ((null? x) #f)
        ((null? (cdr x)) #f)
        ((null? (cdr (cdr x))) #t)
        (else #f)))

(define (revrel rel)
    (cond ((null? rel) '())
        (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel))))))

(define (fun? rel)
    (set? (firsts rel)))

(define (fullfun? fun)
    (set? (seconds fun)))

(define (fullfun? fun)
    (fun? (revrel fun)))