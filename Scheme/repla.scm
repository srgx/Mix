
(include "print.scm")

(define str
  (with-input-from-file "bio.scm"
    (lambda () (read-string 10000))))

(define (find-symbols str symbols)
  (and (not (null? symbols))
       (or (let* ((s (car symbols)) (idx (string-contains str s)))
             (and idx (list s idx)))
           (find-symbols str (cdr symbols)))))

(define replacements
  '(("(" "{") (")" "}")))

(define (subst-replace symbols str)
  (let ((rslt (find-symbols str symbols)))
    (if rslt (let* ((idx (cadr rslt)) (smb (car rslt)))
               (subst-replace symbols
                 (string-replace str (cadr (assoc smb replacements)) idx
                   (+ idx (string-length smb))))) str)))

;(define str "((()))")
(print (subst-replace '("(" ")") str))


