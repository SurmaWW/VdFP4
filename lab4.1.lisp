(defun bubble-sort (sequence &key (key #'identity) (test #'<=))
  (let* ((len (length sequence))
         (arr (map 'vector #'identity sequence))
         (key-cache (make-hash-table :test 'eql)))
    
    (labels ((get-key (item)
               (or (gethash item key-cache)
                   (setf (gethash item key-cache)
                         (funcall key item))))
             
             (should-swap (a b)
               (not (funcall test (get-key a) (get-key b)))))
      
      (dotimes (i (1- len))
        (dotimes (j (- len i 1))
          (when (should-swap (aref arr j) (aref arr (1+ j)))
            (rotatef (aref arr j) (aref arr (1+ j))))))
      
      (coerce arr (type-of sequence)))))

(defun check-bubble-sort (name input expected &key (key #'identity) (test #'<=))
  (format t "~:[FAILED~;passed~]... ~a~%"
          (equal (bubble-sort input :key key :test test) expected)
          name))

(defun test-bubble-sort ()
  (check-bubble-sort "Test 1" '(3 1 2) '(1 2 3))
  (check-bubble-sort "Test 2" '(1 2 3) '(1 2 3))
  (check-bubble-sort "Test 3" '(5 4 3 2 1) '(1 2 3 4 5))
  (check-bubble-sort "Test 4" '(4 1 3 2 4 1) '(1 1 2 3 4 4))
  (check-bubble-sort "Test 5" '(-3 -1 2 5 -4) '(-1 2 -3 -4 5) :key #'abs)
  (check-bubble-sort "Test 6" '(1 2 3 4 5) '(5 4 3 2 1) :test #'>)
  (check-bubble-sort "Test 7" '() '())
  (check-bubble-sort "Test 8" '(42) '(42)))

(test-bubble-sort)