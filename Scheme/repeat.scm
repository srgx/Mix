
(define (repeat value size)
  (define (h c)
    (if (zero? c) '()
        (cons value (h (- c 1)))))
  (h size))

(define (zeros size)
  (repeat 0 size))

(define (ones size)
  (repeat 1 size))
