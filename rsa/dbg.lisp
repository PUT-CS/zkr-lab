(defmacro dbg (var)
  `(format t "~&~a: ~a" ',var ,var))

(defmacro dbg-many (&rest vars)
  `(progn
     ,@(loop for var in vars collect `(format t "~&~a: ~a" ',var ,var))))
