(defun func (i)
    (cond
        ((= i 1) i)
        ((and (> i 1)(< i 10)) 
            (/ (* 2 (sqrt (* (func (- i 1)) i))) 4))
        ((= i 10) 1)
        ((and (> i 10) (< i 21)) 
            (/ (* (func (- i 1)) (log i)) 8))
        (t "Wrong input value")))

(defun test-func (test-info)
    (format t "Testing function F(n) for n = 1 to 20:~%")
    (let ((result-list '(1.000000 0.7071068 0.7282377 0.8533684 1.032817 1.244679 1.475868 1.718062 1.966123
                         1.00000000 0.299736909 0.0931022798 0.0298503291 0.00984709123 0.00333330217 0.00115523450 0.000409128226 0.000147816584 0.0000544046138 0.0000203727072)))
    (loop for i from 1 to 20 do
    (let ((result (func i)))
        (format t " Test~a ~:[FAILED~;passed~]..." i (if (< i 10) (< (abs (- result (nth (- i 1) result-list))) 1e-6) (< (abs (- result (nth (- i 1) result-list))) 1e-8)))
        (if test-info (format t " F~a = ~a~%" i result) (format t "~%"))))))

(test-func t)
