(setf *random-state* (make-random-state t))
(load "modexp.lisp")
(load "dbg.lisp")

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

(defun range (start stop &optional (step 1))
  (loop for i from start below stop by step
        collect i))

(defun get-coprimes (n)
  (loop for i from 1 below n
        when (= 1 (gcd i n))
          collect i))

(defun all-powers (g modulo)
  (map 'list (lambda (x) (mod-exp g x modulo)) (range 1 modulo)))

(defun lists-equal (a b)
  (equal (sort a #'<) (sort b #'<)))

(defun prim-roots (modulo)
  (loop for g from 1 below modulo
        when (lists-equal (get-coprimes modulo) (all-powers g modulo))
          collect g))

(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defvar primes
  (remove-if (lambda (p) (< p 100))
             (plist 1000)))
(defun rand-prime () (nth (random (length primes)) primes))

(defun main ()
  (let* (
         (n (rand-prime))
         (roots (prim-roots n))
         (g (nth (random (length roots)) roots))
         
         (x_ (random-from-range 100 1000))
         (X (mod-exp g x_ n))
         
         (y_ (random-from-range 100 1000))
         (Y (mod-exp g y_ n))
         
         (ka (mod-exp Y x_ n))
         (kb (mod-exp X y_ n))
         )
    (dbg-many n g x_ X y_ Y ka kb)
    (assert (= ka kb))
    t
    ))

(main)
