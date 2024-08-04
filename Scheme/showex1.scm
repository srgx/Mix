
(define (exfun a b . rst)
  (list a b rst))

(display (apply exfun 1 2 '(3 4 5))) (newline)
; (1 3 (4 5))

(display (apply cons '(5 (1 2 3)))) (newline)
;error

(display (apply cons '(5 '(1 2 3)))) (newline)
; (5 1 2 3)


