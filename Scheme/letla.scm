(include "print.scm")

(define-macro (let-lambda f . e)
  `(let ((,(car f) (lambda ,(cdr f) ,(car e))))
     ,(cadr e)))

(let-lambda (f x y z) (+ x y z)
  (print (+ (f 1 2 3) (f 4 5 6))))

