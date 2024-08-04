(include "print.scm")

(define-syntax mif
  (syntax-rules ()
    ((_) #f)
    ((_ cnd thn rst ...)
     (if cnd thn (mif rst ...)))))

(define-syntax mset!
  (syntax-rules ()
    ((_ (car lst) b) (set-car! lst b))))

(do ((i 0 (+ i 1))) ((> i 5)) (print i))

