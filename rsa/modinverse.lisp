(defun mod-inverse (a m)
  (let ((result (find-mod-inverse a m)))
    (if result
        result
        nil)))

(defun find-mod-inverse (a m)
  (let ((extended-gcd (extended-gcd a m)))
    (if (= (car extended-gcd) 1)
        (mod (cadr extended-gcd) m)
        nil)))

(defun extended-gcd (a b)
  (if (= b 0)
      (list a 1 0)
      (let* ((quotient (floor a b))
             (remainder (- a (* quotient b)))
             (previous-gcd-result (extended-gcd b remainder))
             (gcd (car previous-gcd-result))
             (x (cadr previous-gcd-result))
             (y (caddr previous-gcd-result)))
        (list gcd y (- x (* quotient y))))))
