
(define-macro (try-args fst . rst)
  (cons 'or (map (lambda (arg) (cons fst (list arg))) rst)))

