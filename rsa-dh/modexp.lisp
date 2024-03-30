(defun mod-exp (base exponent modulus)
  "Compute (base^exponent) mod modulus efficiently."
  (let ((result 1)
        (base (mod base modulus)))
    (loop for exp from exponent downto 1
          do
             (setf result (mod (* result base) modulus)))
    result))
