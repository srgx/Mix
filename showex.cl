
; -----------------------------------------------

(defun abc (a b &rest rst) (list a b rst))

(write (apply #'abc 1 2 '(4 5 6))) (terpri)

; Cyclone Scheme
; interpret (icyc) - (1 4 (5 6))
; compile (cyclone) - (1 2 (4 5 6))

; -----------------------------------------------

(defun exfun (a b &rest rst)
  (list a b rst))

(write (apply #'exfun 1 2 '(3 4 5))) (terpri)
; (1 3 (4 5))

(write (apply #'cons '(5 (1 2 3)))) (terpri)
;error

(write (apply #'cons '(5 '(1 2 3)))) (terpri)
; (5 1 2 3)

; -----------------------------------------------
