(define (unfold p f g seed)
  (define (h value)
    (if (p value) '()
        (cons (f value) (h (g value)))))
  (h seed))

(define (unfold-right p f g seed)
  (define (h value acc)
    (if (p value) acc
        (h (g value) (cons (f value) acc))))
  (h seed '()))
