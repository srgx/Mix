
(define (abc a b . rst) (list a b rst))

(display (apply abc 1 2 '(4 5 6))) (newline)

; Cyclone Scheme
; interpret (icyc) - (1 4 (5 6))
; compile (cyclone) - (1 2 (4 5 6))
