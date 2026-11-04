
(defun selection-sort (lst &key (key nil) (test #'<))
    (cond
        ((null lst) nil)
        ((= (length lst) 1) lst)
        (t
            (let ((arr (copy-list lst)) (lst-length (length lst)) (current (first lst)) (min (first lst)) (actual-key (if key key #'identity))) 
                (dotimes (i (- lst-length 1))
                    (setq current (nth i arr))
                    (setq min (reduce (lambda (a b) (if (funcall test (funcall actual-key a) (funcall actual-key b)) a b)) (nthcdr i arr)))
                    (if (not (equal current min))
                        (setq arr (append (subseq arr 0 i) (cons min (remove min (nthcdr i arr) :count 1))))))
                arr))))

(defun add-next-reducer (&key (transform nil))
  (lambda (current-item accumulator)
    (let ((transformed-item (if transform 
                                (funcall transform current-item) 
                                current-item)))
      (if (null accumulator)
          (list (cons transformed-item nil))
          (cons (cons transformed-item (caar accumulator)) 
                accumulator)))))

(defun check-my-function (name func input expected )
(format t "~:[FAILED~;passed~]... ~a~%"
(equal (funcall func input) expected) name))

(defun run-tests-selection-sort()
    (check-my-function "Test 1 Selection Sort" #'selection-sort nil nil)
    (check-my-function "Test 2 Selection Sort" #'selection-sort '(1) '(1))
    (check-my-function "Test 3 Selection Sort" #'selection-sort '(3 2 5 5 1 1) '(1 1 2 3 5 5))
    (check-my-function "Test 4 Selection Sort" #'selection-sort '(6 6 6 6 6) '(6 6 6 6 6))
    (format t "~%")
)

(defun run-tests-add-next-reducer()
    (check-my-function "Test 1 Add Next Reducer" (lambda (lst) (reduce (add-next-reducer) lst :from-end t :initial-value nil)) '(1 2 3 4) '((1 . 2) (2 . 3) (3 . 4) (4 . NIL)) )
    (check-my-function "Test 2 Add Next Reducer" (lambda (lst) (reduce (add-next-reducer) lst :from-end t :initial-value nil)) nil nil )
    (check-my-function "Test 3 Add Next Reducer" (lambda (lst) (reduce (add-next-reducer) lst :from-end t :initial-value nil)) '(1) '((1 . NIL)) )
    (check-my-function "Test 4 Add Next Reducer" (lambda (lst) (reduce (add-next-reducer) lst :from-end t :initial-value nil)) '(1 (2 . 2) 3 4 5) '((1 . (2 . 2)) ((2 . 2) . 3) (3 . 4) (4 . 5) (5 . NIL)))
    (check-my-function "Test 5 Add Next Reducer" (lambda (lst) (reduce (add-next-reducer :transform #'1+) lst :from-end t :initial-value nil)) '(1 2 3 4) '((2 . 3) (3 . 4) (4 . 5) (5 . NIL)) )
    (format t "~%")
)

(run-tests-selection-sort)
(run-tests-add-next-reducer)
