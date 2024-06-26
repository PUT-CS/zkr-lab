(setf *random-state* (make-random-state t))

(defun plist (n)
  "return the list of primes not greater than n. 
   build it by means of the sieve of Eratosthenes."
  (do ((arr (make-array (+ n 1) :element-type 'boolean :initial-element t))
       (result (list 2))
       (p      3        (+ p 2)))   ; a candidate possible prime
      ((> p (/ n p))
       (nconc (nreverse result)
              (loop for i from p to n by 2 if (aref arr i) collect i)))
    (when (aref arr p)              ; not marked as composite: p is prime
        (push p result)  
        (loop for i from (* p p) to n by (* 2 p)  ; mark the multiples
              do (setf (aref arr i) nil)))))

(defvar primes
  (remove-if (lambda (p) (< p 1000))
             (plist 10000)))

(defun rand-prime () (nth (random (length primes)) primes))
