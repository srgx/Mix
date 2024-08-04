
(import (srfi 1))

(define stack '())

(define (swap)
  (set! stack
    (append (list (second stack) (first stack))
            (drop stack 2))))

(define (sdrop)
  (set! stack (cdr stack)))

(define (pst)
  (define (h stack)
    (if (null? stack) #t
        (begin (h (cdr stack))
               (write (car stack))
               (display " "))))
  (h stack) (newline))

(define (push v)
  (set! stack (cons v stack)))

(newline)

