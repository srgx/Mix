
(define lists (list (make-list 4 0)
                    (make-list 4 0)))

(define a1 (list "a1" (car lists)))
(define a2 (list "a2" (car lists)))

(list-set! (list-ref lists 0) 0 5)
(list-set! (list-ref a2 1) 1 6)

(display a1) (newline)
(display a2) (newline)

