(setf *random-state* (make-random-state t))
(load "modinverse.lisp")
(load "primes.lisp")
(load "modexp.lisp")
(load "dbg.lisp")

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defun serialize (text)
  (map 'list (lambda (c) (char-code c)) text))

(defun deserialize (charcodes)
  (map 'string (lambda (c) (code-char c)) charcodes))

(defun random-message (len)
  (map 'string (lambda (c) (code-char (random-from-range 65 90))) (make-list len)))

(defun e (phi)
  (loop
    (let ((e (nth (random 1000) primes)))
      (when (= (gcd phi e) 1) (return e)))))

(defun d (e phi) (mod-inverse e phi))

(defun encrypt (char-codes key)
  (format t "encrypting...~%")
  (map 'list (lambda (c) (mod-exp c (car key) (cdr key))) char-codes))

(defun decrypt (char-codes key)
  (format t "decrypting...~%")
  (map 'list (lambda (c) (mod-exp c (car key) (cdr key))) char-codes))

(defun main ()
  (let* ((msg (random-message 10))
         (p (rand-prime))
         (q (rand-prime))
         (n (* p q))
         (phi (* (- p 1) (- q 1)))
         (e (e phi))
         (d (d e phi))
         (public-key (cons e n))
         (private-key (cons d n))
         (c (encrypt (serialize msg) public-key))
         (de (deserialize (decrypt c private-key))))

    (dbg-many msg p q n phi e d c de)
    (assert (string= msg de))))

(time (main))

