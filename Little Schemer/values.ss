;entry 一个点对，里面包含两个长度相同的字列表，且第一个字列表中的s表达式各不相同（即为一个set 集合）

(define lookup-in-entry
    (lambda (name entry entry-f)
        (lookup-in-entry-help name
                                (first entry)
                                (second entry)
                                entry-f)))

(define lookup-in-entry-help
    (lambda (name names values entry-f))
        (cond 
            ((null? names) (entry-f name))
            ((eq? (car names) name) (car values))
            (else (lookup-in-entry-help name 
                                        (cdr names)
                                        (cdr values)
                                        entry-f))))

; continuation-passing style (CPS)
(define lookup-in-table
    (lambda (name table table-f)
        (cond 
            ((null? table) (table-f name))
            (else (lookup-in-table name
                                    (car table)
                                    (lambda (name)
                                        (lookup-in-table name
                                                        (cdr table)
                                                        table-f)))))))

(define value
    (lambda (e)
        (meaning e '())))

(define meaning
    (lambda (e table)
        ((expression-to-action e) e table)))   


(define apply-primitive
    (lambda (name vals)
        (cond 
            ((eq? name 'cons) (cons (first vals) (second vals)))
            ((eq? name 'car) (car (first vals)))
            ((eq? name 'cdr) (cdr (first vals)))
            ((eq? name 'null?) (null? (first vals)))
            ((eq? name 'eq?) (eq? (first vals) (second vals)))
            ((eq? name 'atom?) (atom? (first vals)))
            ((eq? name 'zero?) (zero? (first vals)))
            ((eq? name 'add1) (add1 (first vals)))
            ((eq? name 'sub1) (sub1 (first vals)))
            ((eq? name 'number?) (number? (first vals))))))                                                

(define apply-closure 
    (lambda (closure vals)
        (meaning (body-of closure) ( extend-table
        (new-entry (formals-of closure) vals)
        (table-of closure)))))