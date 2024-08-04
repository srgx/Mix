
(defmacro let-lambda (f &rest e)
  `(let ((,(car f) (lambda ,(cdr f) ,(car e))))
     ,(cadr e)))

(let-lambda (f x y z) (+ x y z)
  (write (= 21 (+ (funcall f 1 2 3) (funcall f 4 5 6)))))

