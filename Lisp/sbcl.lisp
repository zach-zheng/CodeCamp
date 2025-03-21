; 迭代show-squares do : (variable initial update)

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

; 接受一个列表，并返回 a 在列表里所出现的次数。

(defun counter-a (x lst)
   (if (null lst) 
      0
      (eql (car lst) x) (+ x 1)
      (let z (+ counter-a x (cdr lst) 1))))


; 递归版本的 show-squares
; progn 接受任意数量的表达式，依序求值，并返回最后一个表达式的值。
(defun show-squares (i end)
   (if (> i end)
     'done
     (progn
       (format t "~A ~A~%" i (* i i))
       (show-squares (+ i 1) end))))

; 列印从 start 到 end 之间的整数的平方：
; > (show-squares 2 5)
; 2 4
; 3 9
; 4 16
; 5 25
; DONE
