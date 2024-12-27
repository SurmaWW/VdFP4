(defun rpropagation-reducer (&key (comparator #'<)) ; Ключовий параметр comparator
  (lambda (current acc)
    (let ((best-so-far (car acc))) ;  Поточний "кращий" елемент (правий)
      ;; Якщо current не "кращий" за best-so-far, замінюємо його
      (cons (if (funcall comparator best-so-far current)
                best-so-far  
                current)    
            acc))))          

(defun apply-rpropagation-reducer (list &key (comparator #'<)) ; Ключовий параметр comparator
  ;; Reduce обходить список з кінця
  (butlast                       
    (reduce (rpropagation-reducer :comparator comparator) ; 
            list
            :from-end t          ; Обхід списку з кінця
            :initial-value (list (car (last list)))))) ; Initial-value — останній елемент списку

(defun check-rpropagation (name input expected &key (comparator #'<))
 (format t "~:[FAILED~;passed~]... ~a~%    Input: ~a~%    Expected: ~a~%    Got: ~a~%~%" 
   (equal (apply-rpropagation-reducer input :comparator comparator) expected)
   name
   input 
   expected
   (apply-rpropagation-reducer input :comparator comparator)))

(defun test-rpropagation ()
 (check-rpropagation "Normal" '(5 7 1 6 3) '(1 1 1 3 3))
 (check-rpropagation "Reverse" '(5 7 1 6 3) '(7 7 6 6 3) :comparator #'>)
 (check-rpropagation "Fail Test" '(1 3 2 4 3) '(4 4 4 4 3))
 (check-rpropagation "Same" '(2 2 2 2 2) '(2 2 2 2 2)) 
 (check-rpropagation "Fractions normal" '(1.5 3.7 1.6 2.9) '(1.5 1.6 1.6 2.9))
 (check-rpropagation "Fractions reverse" '(1.5 3.7 1.6 2.9) '(3.7 3.7 2.9 2.9) :comparator #'>)
 (check-rpropagation "Large numbers" '(1000000 9999999 1000001) '(1000000 1000001 1000001))
 (check-rpropagation "With Zero" '(0 1 0) '(1 1 0) :comparator #'>))