(setf *random-state* (make-random-state t))
(load "modinverse.lisp")
(load "primes.lisp")
(load "modexp.lisp")
(load "dbg.lisp")

(defun serialize (text)
  (map 'list (lambda (c) (char-code c)) text))

(defun deserialize (charcodes)
  (map 'string (lambda (c) (code-char c)) charcodes))

(defun e (phi)
  (loop
    (let ((e (nth (random 1000) primes)))
      (when (= (gcd phi e) 1) (return e)))))

(defun d (e phi) (mod-inverse e phi))

(defun encrypt (char-codes key)
  (print "encrypting...")
  (map 'list (lambda (c) (mod-exp c (car key) (cdr key))) char-codes))

(defun decrypt (char-codes key)
  (print "decrypting...")
  (map 'list (lambda (c) (mod-exp c (car key) (cdr key))) char-codes))

(defun main ()
  (when (< (length sb-ext:*posix-argv*) 2) (print "No arg")(exit))
  (let* ((msg (nth 1 sb-ext:*posix-argv*))
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

    (dbg-many msg p q n phi e d c de)))

(main)

