(import (srfi 1) (srfi 95))

(include "assoc.scm")
(include "print.scm")

(define data
  '(2 2 2 3 3 3 3 3 4 4
    4 5 5 5 5 5 5 6 6 7))

(define (occurrences d)
  (sort
    (let h ((d d) (acc '()))
      (if (null? d) acc
          (h (cdr d) (update-assoc-flex (car d) acc))))
    (lambda (a b) (< (car a) (car b)))))

(define (frequencies data)
  (let ((ld (length data)))
    (fold-right
      (lambda (x y)
        (cons (list (car x) (* (/ (cadr x) ld) 100)) y))
      '() (occurrences data))))

(define (cumulative-frequencies frqs)
  (define (h frqs acc)
    (if (null? frqs) '()
        (let ((nv (+ (cadar frqs) acc)))
          (cons (list (caar frqs) nv)
                (h (cdr frqs) nv)))))
  (h frqs 0))

(define (median v)
  (let* ((le (vector-length v))
         (h (quotient le 2))
         (vh (vector-ref v h)))
    (if (even? le) (/ (+ vh (vector-ref v (- h 1))) 2) vh)))

(define (percentile k v)
  (define (percentile-index k v)
     (let* ((va (* k (+ (vector-length v) 1)))
            (qo (quotient va 100))
            (sq (- qo 1))
            (re (remainder va 100)))
       (if (zero? re) (list sq) (list sq qo))))
  (percentile-index k v))

(print
  (let ((d 1e-5)
        (frqs '((2 15) (3 25) (4 15) (5 30) (6 10) (7 5)))
        (ocs '((2 3) (3 5) (4 3) (5 6) (6 2) (7 1)))
        (cfrqs '((2 15) (3 40) (4 55) (5 85) (6 95) (7 100))))
    (and (equal? (occurrences data) ocs)
         (equal? (frequencies data) frqs)
         (equal? (cumulative-frequencies frqs) cfrqs)
         (< (abs (- 7.5 (median (vector 5 6 7 8 9 10)))) d)
         (< (abs (- 8 (median (vector 4 7 8 10 15)))) d))))

(print
  (let ((veo (vector 2 4 8 16 20 30 50 120 130)))
    (list (percentile 50 veo) (percentile 99 veo))))



